package net.virtualvoid.codecs.zstd

import net.virtualvoid.codecs.Utils
import net.virtualvoid.codecs.Utils._
import scodec.bits.{ BitVector, ByteVector, HexStringSyntax }
import scodec.codecs._
import scodec.{ Attempt, Codec, DecodeResult, SizeBound }
import shapeless._

import java.io.FileInputStream

object Zstd {
  case class Frame(
      header:   FrameHeader,
      blocks:   Seq[Block],
      checksum: Option[Long]
  )

  lazy val frame: Codec[Frame] =
    frameHeader.flatPrepend { header =>
      val blocks =
        Utils.collectWithState[Block, BlockState](BlockState.initial) { state =>
          block(state).flatZip { // FIXME: other cases
            case nextBlock: CompressedBlock =>
              provide {
                if (nextBlock.blockHeader.lastBlock) None
                else Some {
                  val header = nextBlock.sequences.header
                  BlockState(
                    nextBlock.literals.huffmanSpec,
                    if (header.litLengthMode == 2) Some(header.litLengthTable) else state.litLenTable,
                    if (header.matchLengthMode == 2) Some(header.matchLengthTable) else state.matchLenTable,
                    if (header.offsetMode == 2) Some(header.offsetTable) else state.offsetTable
                  )
                }
              }
          }
        }

      val checksum: Codec[Option[Long]] = conditional(header.headerDesc.contentChecksum, uint32)

      blocks :: checksum
    }.as[Frame]

  case class FrameHeader(
      headerDesc:       FrameHeaderDesc,
      frameContentSize: Long,
      windowSize:       Long
  )

  lazy val frameHeader: Codec[FrameHeader] = constant(hex"28B52FFD") ~> frameHeaderDesc.flatMapD { desc =>
    val frameContentSizeLength = desc.frameContentSizeFlag match {
      case 0 if desc.singleSegment => 1
      case 0                       => 0
      case 1                       => 2
      case 2                       => 4
      case 3                       => 8
    }
    val fcsBase = if (frameContentSizeLength == 2) 256 else 0

    //println(desc)
    //println(s"fcs_length: $frameContentSizeLength")
    val frameContentSize = {
      if (frameContentSizeLength > 0)
        ulongL(frameContentSizeLength * 8).xmap[Long](_ + fcsBase, _ - fcsBase)
      else
        provide(-1L)
    }
    lazy val windowSize: Codec[Long] =
      uint8.xmap[Long](_.toLong /** FIXME: needs to be split into exponent / mantissa */ , _ => ???)

    if (desc.singleSegment)
      (provide(desc) :: frameContentSize).xmap[FrameHeader]({
        case desc :: fcs :: HNil => FrameHeader(desc, fcs, fcs)
      }, _ => ???)
    else
      (provide(desc) :: frameContentSize :: windowSize).as[FrameHeader]

  }

  case class FrameHeaderDesc(
      frameContentSizeFlag: Int,
      singleSegment:        Boolean,
      contentChecksum:      Boolean,
      dictionaryIdFlag:     Int
  )

  lazy val frameHeaderDesc: Codec[FrameHeaderDesc] =
    (uint(2) :: bool :: constant(BitVector.zero) :: constant(BitVector.zero) :: bool :: uint(2)).as[FrameHeaderDesc]

  sealed trait Block {
    def blockHeader: BlockHeader
  }
  case class CompressedBlock(
      blockHeader: BlockHeader,
      literals:    Literals,
      sequences:   Sequences
  ) extends Block
  case class LiteralSpec(
      literalsType:    Int,
      regeneratedSize: Int,
      compressedSize:  Int,
      numStreams:      Int
  )
  case class Literals(
      spec:        LiteralSpec,
      huffmanSpec: Option[HuffmanSpec],
      data:        ByteVector
  )

  sealed trait Offset
  case class DirectOffset(offset: Int) extends Offset
  case class RepeatedOffset(idx: Int) extends Offset

  case class Sequence(literalLen: Int, matchLen: Int, offset: Offset)
  case class Sequences(
      header:    SequenceSectionHeader,
      sequences: Seq[Sequence]
  )

  /** State that needs to be maintained between blocks */
  case class BlockState(
      huffmanSpec:   Option[HuffmanSpec],
      litLenTable:   Option[FSETableSpec],
      matchLenTable: Option[FSETableSpec],
      offsetTable:   Option[FSETableSpec]
  )
  object BlockState {
    def initial: BlockState = BlockState(None, None, None, None)
  }

  def block(blockState: BlockState): Codec[Block] =
    blockHeader.flatMapD { header =>
      require(header.blockType == 2) // compressed block
      variableSizeBytes(provide(header.blockSize), compressedBlock(blockState, header).widen[Block](b => b, _ => ???))
    }

  case class BlockHeader(
      lastBlock: Boolean,
      blockType: Int,
      blockSize: Int
  )
  lazy val blockHeader: Codec[BlockHeader] =
    variableSizeBytes(
      provide(3),
      reversed { // weird little endian bit order
        withReversedBits {
          (uint(21) :: uint(2) :: bool).mapD[BlockHeader] {
            case size :: tpe :: last :: HNil =>
              BlockHeader(last, tpe, size)
          }
        }
      }
    )

  def compressedBlock(blockState: BlockState, header: BlockHeader): Codec[CompressedBlock] = {
    require(header.blockType == 2) // compressed block
    (provide(header) :: literals :: sequences(blockState)).as[CompressedBlock]
  }

  lazy val literals: Codec[Literals] =
    literalSpec.flatPrepend { spec =>
      spec.literalsType match {
        case 0 => provide(None: Option[HuffmanSpec]) :: bytes(spec.compressedSize)
        case 2 => variableSizeBytes(provide(spec.compressedSize), compressedLiterals(spec))
      }
    }.as[Literals]

  lazy val literalSpec: Codec[LiteralSpec] = {
    // FIXME: hard to decode because of weird bit order
    (uint4 :: uint2 :: uint2).flatMapD {
      case lenBits :: sizeFormat :: tpe :: HNil =>
        //require(tpe == 0, s"Literal type == $tpe but only 0 supported") // raw, FIXME: support more
        tpe match {
          case 0 => // raw literals
            val len = sizeFormat match {
              case 0 => lenBits << 1
              case 2 => (lenBits << 1) + 1
            }
            (provide(tpe) :: provide(len) :: provide(len) :: provide(1)).as[LiteralSpec]
          case 2 => // Compressed_Literals_Block
            def sizes(sizeFormat: Int): Codec[Int :: Int :: Int :: HNil] =
              sizeFormat match {
                case 0 | 1 =>
                  val numStreams = if (sizeFormat == 0) 1 else 4
                  uint16L.flatMapD { len =>
                    val regenSize = ((len & 0x3f) << 4) + lenBits
                    val compSize = len >> 6
                    //println(s"lenBits: ${lenBits.toHexString} len: ${len.toHexString} regenSize: $regenSize compSize: $compSize ${(len & 0x3f).toHexString}")
                    provide(regenSize) :: provide(compSize) :: provide(numStreams)
                  }
                case 2 =>
                  uint24L.flatMapD { len =>
                    val regenSize = ((len & 0x3ff) << 4) + lenBits
                    val compSize = len >> 10
                    provide(regenSize) :: provide(compSize) :: provide(4)
                  }
              }

            (provide(tpe) :: sizes(sizeFormat)).as[LiteralSpec]
        }
    }
  }

  case class HuffmanSpec(maxNumberOfBits: Int, weights: Seq[Int]) {
    lazy val maxWeight = weights.max
    override def toString: String = s"Huffman weights total: ${weights.sum} maxNumberOfBits: $maxNumberOfBits max: $maxWeight\n" + weights.zipWithIndex.collect {
      case (weight, idx) if weight > 0 => f"$idx%2x ${Utils.char(idx)} weight $weight%2d bits ${maxNumberOfBits + 1 - weight}%2d"
    }.mkString("\n")

    def toTable: HuffmanTable = {
      val sorted = weights.zipWithIndex.filter(_._1 > 0).sorted
      val first = sorted.head
      val firstBits = maxNumberOfBits + 1 - first._1
      val entries =
        sorted.tail.scanLeft(HuffmanEntry(first._2, first._1, firstBits, 0, 0, (1 << firstBits) - 1)) { (last, next) =>
          val weight = next._1
          val sym = next._2
          val numberOfBits = maxNumberOfBits + 1 - weight
          val nextNum = (last.code + 1) >> (weight - last.weight)

          HuffmanEntry(sym, weight, numberOfBits, nextNum, nextNum << (maxNumberOfBits - numberOfBits), ((1 << numberOfBits) - 1) << (maxNumberOfBits - numberOfBits))
        }
      HuffmanTable(maxNumberOfBits, entries)
    }
  }
  object HuffmanSpec {
    def apply(collectedWeights: Seq[Int]): HuffmanSpec = {
      val totalWeights =
        collectedWeights.collect {
          case x if x > 0 => 1 << (x - 1)
        }.sum
      val nextPowerOf2 = java.lang.Integer.highestOneBit(totalWeights) << 1
      val maxNumberOfBits = java.lang.Integer.numberOfTrailingZeros(nextPowerOf2)
      val remainingBins = nextPowerOf2 - totalWeights
      val lastWeight = java.lang.Integer.numberOfTrailingZeros(remainingBins) + 1
      println(s"totalWeights: $totalWeights nextPowerOf2: $nextPowerOf2 remaining: $remainingBins last weight: $lastWeight")

      new HuffmanSpec(maxNumberOfBits, collectedWeights :+ lastWeight)
    }
  }
  case class HuffmanEntry(symbol: Int, weight: Int, numberOfBits: Int, code: Int, shiftedCode: Int, mask: Int) {
    def toString(maxNumberOfBits: Int): String = f"${binStringWithLeftZeros(code, numberOfBits)}%10s ${binStringWithLeftZeros(shiftedCode, maxNumberOfBits)} ${mask.toBinaryString} ${numberOfBits}%2d bits => ${Utils.char(symbol)}"
  }

  case class HuffmanTable(maxNumberOfBits: Int, entries: Seq[HuffmanEntry]) {
    override def toString: String =
      entries.map(_.toString(maxNumberOfBits)).mkString("\n")

    /** code should contain `maxNumberOfBits` of new data */
    def read(code: Int): HuffmanEntry =
      entries.find { e =>
        //val masked = code & e.mask
        //println(s"${masked.toBinaryString} ${e.shiftedCode.toBinaryString} ${(code & e.mask) == e.code} at ${e.toString(maxNumberOfBits)}")
        (code & e.mask) == e.shiftedCode
      }.get
  }
  private def binStringWithLeftZeros(num: Int, numberOfBits: Int): String = {
    val str = num.toBinaryString
    "0" * (numberOfBits - str.size) + str
  }

  def ifEnoughDataAvailable[T](inner: Codec[T]): Codec[Option[T]] =
    lookahead(inner.mapD[Unit](_ => ())).flatMapD[Option[T]] { canRead =>
      conditional(canRead, inner)
    }

  lazy val huffmanSpec: Codec[HuffmanSpec] = {
    fseTableSpec.flatMapD { tableSpec =>
      val table = tableSpec.toTable((0 to 20).map(provide))
      println(s"Huffman table weights FSE: $table")
      def nextValues(state1: Int, state2: Int, current: Seq[Int]): Codec[HuffmanSpec] = {
        val v1Entry = table.entries(state1)
        val v2Entry = table.entries(state2)
        println(s"Read entries ${v1Entry.symbol} ${v2Entry.symbol}")

        ifEnoughDataAvailable(readExtra(v1Entry.nbBits) :: readExtra(v2Entry.nbBits)).flatMapD {
          case Some(newState1 :: newState2 :: HNil) =>
            nextValues(newState1 + v1Entry.offset, newState2 + v2Entry.offset, current ++ Seq(v1Entry.symbol, v2Entry.symbol))
          case None =>
            ifEnoughDataAvailable(readExtra(v1Entry.nbBits)).flatMapD {
              case Some(newState1) =>
                provide(HuffmanSpec(current ++ Seq(v1Entry.symbol, v2Entry.symbol, table.entries(newState1).symbol)))
              case None => provide(HuffmanSpec(current ++ Seq(v1Entry.symbol, v2Entry.symbol)))
            }
        }
      }

      reversed {
        withReversedBits {
          (padding ~> uint(tableSpec.accuracyLog) :: uint(tableSpec.accuracyLog)).flatMapD {
            case state1 :: state2 :: HNil =>
              println(s"Start states state1: $state1 state2: $state2")
              nextValues(state1, state2, Nil)
          }
        }
      }
    }
  }
  def padding: Codec[Unit] =
    peek(uint8).flatMapD { first =>
      val padding = java.lang.Integer.numberOfLeadingZeros(first) - 24 /* uint8 as 32bit int */ + 1

      ignore(padding)
    }

  def compressedLiterals(spec: LiteralSpec): Codec[Option[HuffmanSpec] :: ByteVector :: HNil] =
    variableSizeBytes(uint8, huffmanSpec).flatMapD { huffmanSpec =>
      println(s"Lit Spec: $spec Huffman Spec: $huffmanSpec")
      println(huffmanSpec.toTable)

      provide(Some(huffmanSpec): Option[HuffmanSpec]) :: {
        if (spec.numStreams == 4)
          (uint16L :: uint16L :: uint16L).flatMapD {
            case stream1 :: stream2 :: stream3 :: HNil =>
              val numEls = (spec.regeneratedSize + 3) / 4
              val remaining = spec.regeneratedSize - 3 * numEls

              (variableSizeBytes(provide(stream1), decodeLiterals(huffmanSpec, numEls)) ::
                variableSizeBytes(provide(stream2), decodeLiterals(huffmanSpec, numEls)) ::
                variableSizeBytes(provide(stream3), decodeLiterals(huffmanSpec, numEls)) ::
                decodeLiterals(huffmanSpec, remaining)).mapD[ByteVector] {
                  case s1 :: s2 :: s3 :: s4 :: HNil => s1 ++ s2 ++ s3 ++ s4
                }
          }
        else
          decodeLiterals(huffmanSpec, spec.regeneratedSize)
      }
    }

  def decodeLiterals(huffmanSpec: HuffmanSpec, numElements: Int): Codec[ByteVector] = {
    val table = huffmanSpec.toTable

    reversed {
      withReversedBits {
        appendInput(ByteVector(0)) {
          def readOne: Codec[Int] =
            peek(uint(table.maxNumberOfBits)).flatMapD { v =>
              //println(s"Read ${v.toBinaryString}")
              val entry = table.read(v)
              //println(s"${char(entry.symbol)} Found entry: $entry")

              ignore(entry.numberOfBits) ~> provide(entry.symbol)
            }

          padding ~> vectorOfN(provide(numElements), readOne).mapD[ByteVector](bs => ByteVector(bs: _*))
        }
      }
    }
  }

  def readExtra(bits: Int): Codec[Int] = if (bits == 0) provide(0) else uint(bits)

  def sequences(blockState: BlockState): Codec[Sequences] =
    sequenceSectionHeader(blockState).flatMapD { header =>
      val litLenTable = header.litLengthTable.toTable(LitLenCodeTable)
      val matchLenTable = header.matchLengthTable.toTable(MatchLenCodeTable)
      val offsetTable = header.offsetTable.toTable(OffsetCodeTable)

      println("Litlen")
      println(header.litLengthTable.histogram)
      println(litLenTable)
      println("MatchLen")
      println(header.matchLengthTable.histogram)
      println(matchLenTable)
      println("Offset")
      println(header.offsetTable.histogram)
      println(offsetTable)

      reversed {
        withReversedBits {
          (padding ~> litLenTable.decodeInitialState :: offsetTable.decodeInitialState :: matchLenTable.decodeInitialState).flatMapD {
            case litLenState :: offsetState :: matchLenState :: HNil =>
              println(litLenState, offsetState, matchLenState)

              val seqs = Utils.collectWithState(header.numberOfSequences :: litLenState :: matchLenState :: offsetState :: HNil) {
                case remaining :: litLenState :: matchLenState :: offsetState :: HNil =>
                  val nextSeq =
                    (offsetState.decodeSymbol :: matchLenState.decodeSymbol :: litLenState.decodeSymbol).mapD {
                      case offset :: matchLen :: litLen :: HNil => Sequence(litLen, matchLen, offset)
                    }

                  val nextState = conditional(remaining > 1, provide(remaining - 1) :: litLenState.decodeNextState :: matchLenState.decodeNextState :: offsetState.decodeNextState)

                  nextSeq ~ nextState
              }

              (provide(header) :: seqs).as[Sequences]
          }
        }
      }
    }

  case class SequenceSectionHeader(
      numberOfSequences: Int,
      litLengthMode:     Int,
      offsetMode:        Int,
      matchLengthMode:   Int,
      litLengthTable:    FSETableSpec,
      offsetTable:       FSETableSpec,
      matchLengthTable:  FSETableSpec
  )

  def sequenceSectionHeader(blockState: BlockState): Codec[SequenceSectionHeader] =
    "sequenceSectionHeader" | (numberOfSequences :: uint2 :: uint2 :: uint2 :: ("reserved sequence section modes" | constant(BitVector.bits(Iterable(false, false))))).flatConcat {
      case num :: lMode :: oMode :: mLMode :: _ :: HNil =>
        def tableFor(mode: Int, defaultSpec: FSETableSpec, previous: BlockState => Option[FSETableSpec]): Codec[FSETableSpec] = {
          mode match {
            case 0 => provide(defaultSpec)
            case 2 => fseTableSpec
            case 3 => provide(previous(blockState).getOrElse(throw new IllegalStateException("Treeless_Literals_Block but previous instance was missing")))
          }
        }
        println(s"num seqs $num modes $lMode $oMode $mLMode")

        tableFor(lMode, DefaultLitLenTable, _.litLenTable) :: tableFor(oMode, DefaultOffsetTable, _.offsetTable) :: tableFor(mLMode, DefaultMatchLenTable, _.matchLenTable)
    }
      .as[SequenceSectionHeader]

  lazy val numberOfSequences: Codec[Int] =
    uint8.flatMapD[Int] {
      case 0            => provide(0)
      case x if x < 128 => provide(x)
      case x if x < 255 =>
        // ((byte0-128) << 8) + byte1
        uint8.mapD(byte1 => ((x - 128) << 8) + byte1)
      case 255 =>
        // byte1 + (byte2<<8) + 0x7F00
        uint16L.mapD(_ + 0x7f00)
    }

  case class FSETableSpec(
      accuracyLog: Int,
      histogram:   Seq[Int]
  ) {
    def toTable[T](codeTable: Seq[Codec[T]]): FSETable[T] = {
      val size = 1 << accuracyLog
      val mask = size - 1
      val skip = (size >> 1) + (size >> 3) + 3
      val minMinus1 = size - histogram.count(_ == -1)
      val buffer = new Array[FSETableEntry[T]](size)

      var lastIdx = size - 1
      histogram.zipWithIndex.filter(_._1 == -1).foreach {
        case (_ /* -1 */ , symbol) =>
          require(buffer(lastIdx) == null)
          buffer(lastIdx) = FSETableEntry(symbol, codeTable(symbol), accuracyLog, 0)
          lastIdx -= 1
      }

      var pos = 0
      def allocate(entry: FSETableEntry[T]): Unit = {
        buffer(pos) = entry
        do {
          pos = (pos + skip) & mask
        } while (pos >= minMinus1)
      }

      histogram.zipWithIndex.foreach {
        case (count, symbol) =>
          if (count > 0) {
            val e = FSETableEntry(symbol, codeTable(symbol), -1, -1) // bits and offset to be filled later
            (0 until count).foreach { _ => allocate(e) }

            val nextPower =
              if (count == 1) 1
              else java.lang.Integer.highestOneBit(count - 1) << 1

            val width = size / nextPower
            val bits = java.lang.Integer.numberOfTrailingZeros(width)
            val double = nextPower - count

            println(s"Symbol: $symbol count: $count nextPower: $nextPower width: $width bits: $bits double: $double single: ${count - double}")

            val doubleStartOffset = (count - double) * width
            buffer.zipWithIndex.filter(e => e._1 != null && e._1.symbol == symbol).zipWithIndex.foreach {
              case ((s, idx), num) =>
                val newEntry =
                  if (num < double) // double entry
                    s.copy(nbBits = bits + 1, offset = doubleStartOffset + 2 * width * num)
                  else // single entry
                    s.copy(nbBits = bits, offset = width * (num - double))

                buffer(idx) = newEntry
            }
          }
      }

      FSETable(accuracyLog, buffer.toIndexedSeq)
    }
  }
  case class FSETableEntry[T](
      symbol:       Int,
      decodeSymbol: Codec[T],
      nbBits:       Int,
      offset:       Int
  )

  trait FSEState[T] {
    def decodeSymbol: Codec[T]
    def decodeNextState: Codec[FSEState[T]]
  }
  case class FSETable[T](accuracyLog: Int, entries: IndexedSeq[FSETableEntry[T]]) {
    def decodeInitialState: Codec[FSEState[T]] = uint(accuracyLog).mapD(states)

    private val states: IndexedSeq[FSEState[T]] = entries.map { e =>
      new FSEState[T] {
        override def decodeSymbol: Codec[T] = e.decodeSymbol
        override def decodeNextState: Codec[FSEState[T]] = readExtra(e.nbBits).mapD(bs => states(e.offset + bs))

        override def toString: String = s"State ${e.symbol}"
      }
    }

    override def toString: String =
      entries.zipWithIndex.map {
        case (null, state) => f"$state%4d null"
        case (FSETableEntry(symbol, codec, nbBits, offset), state) =>
          f"$state%4d $nbBits%2d bits at $offset%4d => $symbol"
      }.mkString("\n")
  }

  lazy val DefaultLitLenTable =
    FSETableSpec(6, Seq(4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 1, 1, -1, -1, -1, -1))

  lazy val DefaultMatchLenTable =
    FSETableSpec(6, Seq(1, 4, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1))

  lazy val DefaultOffsetTable =
    FSETableSpec(5, Seq(1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1))

  /*
  | `Match_Length_Code` |         0-31            |
  | ------------------- | ----------------------- |
  | value               | `Match_Length_Code` + 3 |
  | `Number_of_Bits`    |          0              |

  | `Match_Length_Code` |  32  |  33  |  34  |  35  |  36  |  37  |  38  |  39  |
  | ------------------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
  | `Baseline`          |  35  |  37  |  39  |  41  |  43  |  47  |  51  |  59  |
  | `Number_of_Bits`    |   1  |   1  |   1  |   1  |   2  |   2  |   3  |   3  |

  | `Match_Length_Code` |  40  |  41  |  42  |  43  |  44  |  45  |  46  |  47  |
  | ------------------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
  | `Baseline`          |  67  |  83  |  99  |  131 |  259 |  515 | 1027 | 2051 |
  | `Number_of_Bits`    |   4  |   4  |   5  |   7  |   8  |   9  |  10  |  11  |

  | `Match_Length_Code` |  48  |  49  |  50  |  51  |  52  |
  | ------------------- | ---- | ---- | ---- | ---- | ---- |
  | `Baseline`          | 4099 | 8195 |16387 |32771 |65539 |
  | `Number_of_Bits`    |  12  |  13  |  14  |  15  |  16  |
   */

  lazy val OffsetCodeTable: IndexedSeq[Codec[Offset]] =
    (0 to 31).map { offsetCode =>
      readExtra(offsetCode).mapD { offsetExtra =>
        val offsetValue = (1 << offsetCode) + offsetExtra

        if (offsetValue > 3) DirectOffset(offsetValue - 3)
        else RepeatedOffset(offsetValue)
      }
    }

  lazy val MatchLenCodeTable: IndexedSeq[Codec[Int]] =
    intCodeTableWithExtraBits(3, Vector.fill(32)(0) ++ Vector(1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))

  /*
  | `Literals_Length_Code` |         0-15           |
  | ---------------------- | ---------------------- |
  | length                 | `Literals_Length_Code` |
  | `Number_of_Bits`       |          0             |

  | `Literals_Length_Code` |  16  |  17  |  18  |  19  |  20  |  21  |  22  |  23  |
  | ---------------------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
  | `Baseline`             |  16  |  18  |  20  |  22  |  24  |  28  |  32  |  40  |
  | `Number_of_Bits`       |   1  |   1  |   1  |   1  |   2  |   2  |   3  |   3  |

  | `Literals_Length_Code` |  24  |  25  |  26  |  27  |  28  |  29  |  30  |  31  |
  | ---------------------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
  | `Baseline`             |  48  |  64  |  128 |  256 |  512 | 1024 | 2048 | 4096 |
  | `Number_of_Bits`       |   4  |   6  |   7  |   8  |   9  |  10  |  11  |  12  |

  | `Literals_Length_Code` |  32  |  33  |  34  |  35  |
  | ---------------------- | ---- | ---- | ---- | ---- |
  | `Baseline`             | 8192 |16384 |32768 |65536 |
  | `Number_of_Bits`       |  13  |  14  |  15  |  16  |
   */

  lazy val LitLenCodeTable: IndexedSeq[Codec[Int]] =
    intCodeTableWithExtraBits(0, Vector.fill(16)(0) ++ Vector(1, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))

  def intCodeTableWithExtraBits(base: Int, extraBits: Vector[Int]): IndexedSeq[Codec[Int]] = {
    case class Code(baseline: Int, extraBits: Int)

    val first = extraBits.head
    extraBits.tail
      .scanLeft(Code(base, first)) { (code, nextBits) =>
        Code(code.baseline + (1 << code.extraBits), nextBits)
      }
      .map(c => readExtra(c.extraBits).mapD(_ + c.baseline))
  }

  /**
   * Reverses bits while running inner codec
   */
  def withReversedBits[T](inner: Codec[T]): Codec[T] = mapInputBits(_.reverseBitOrder)(inner)
  def reversed[T](inner: Codec[T]): Codec[T] = mapInputBits(_.reverse)(inner)
  def appendInput[T](byteVector: ByteVector)(inner: Codec[T]): Codec[T] =
    mapInputBits(_ ++ byteVector.toBitVector, _.dropRight(byteVector.length * 8))(inner)

  def mapInputBits[T](bi: BitVector => BitVector)(inner: Codec[T]): Codec[T] = mapInputBits(bi, bi)(inner)
  def mapInputBits[T](forward: BitVector => BitVector, backward: BitVector => BitVector)(inner: Codec[T]): Codec[T] =
    new Codec[T] {
      override def decode(bits: BitVector): Attempt[DecodeResult[T]] =
        inner.decode(forward(bits)).map(_.mapRemainder(backward))
      override def encode(value: T): Attempt[BitVector] =
        inner.encode(value).map(backward)
      override def sizeBound: SizeBound = inner.sizeBound
    }

  lazy val fseTableSpec: Codec[FSETableSpec] = withReversedBits {
    byteAligned {
      /**
       * Reads `bits` from a little endian bit stream and interprets them reversed
       */
      def uintLEBits(bits: Int): Codec[Int] =
        limitedSizeBits(bits, reversed(uint(bits)))

      case class ReadState(accuracyLog: Int, remaining: Int, counts: Seq[Int]) {
        def next: Codec[FSETableSpec] =
          if (remaining <= 1) {
            val res = FSETableSpec(accuracyLog, counts)
            provide(res)
          } else {
            val threshold = java.lang.Integer.highestOneBit(remaining)
            val max = (threshold << 1) - 1 - remaining
            val bits = java.lang.Integer.numberOfTrailingZeros(threshold)

            // use peek because we don't know yet if `bits` or `bits + 1` bits will be used
            peek(uintLEBits(bits + 1)).flatMapD { bitsRead =>
              val low = bitsRead & (threshold - 1)

              val (value, bitsUsed) =
                if (low < max)
                  (low, bits)
                else {
                  val v = if (bitsRead >= threshold) bitsRead - max else bitsRead
                  (v, bits + 1)
                }

              val realCount = value - 1
              val read = if (realCount == -1) 1 else realCount
              //println(s"accuracyLog: $accuracyLog remaining: $remaining threshold: $threshold max: $max bitsRead: $bitsRead value: $value bitsUsed: $bitsUsed realCount: $realCount")
              val nextState = ReadState(accuracyLog, remaining - read, counts :+ realCount)

              ignore(bitsUsed) ~> (if (realCount == 0) nextState.afterZero else nextState.next)
            }
          }

        def afterZero: Codec[FSETableSpec] =
          uintLEBits(2).flatMapD { num =>
            val repeatCounts = Vector.fill(num)(0)
            val nextState = copy(counts = counts ++ repeatCounts)
            if (num == 3) // next repeat
              nextState.afterZero
            else
              nextState.next
          }
      }

      uintLEBits(4).flatMapD { log0 =>
        val log = log0 + 5
        val states = 1 << log

        ReadState(log, states + 1, Vector.empty).next
      }
    }
  }

  def peekPrintNextBits(num: Int, tag: String): Codec[Unit] =
    peek(bits(num)).mapD { bv => println(s"$tag: ${bv.toBin}") }

  def peekPrintNextBytes(num: Int, tag: String): Codec[Unit] =
    peek(bytes(num)).mapD { bv => println(s"$tag: ${bv.toHex}") }

  def peekRemainingBytes(tag: String): Codec[Unit] =
    peek(bytes.mapD { bv => println(s"$tag: ${bv.size} bytes ${bv.toHex}") })
}

object ZstdTest extends App {
  val data = {
    val fis = new FileInputStream("abc_times_100_with_middle_d.txt.zst")
    //val fis = new FileInputStream("repeated.txt.19.zst")
    val buffer = new Array[Byte](10000)
    val read = fis.read(buffer)
    require(read > 0)
    ByteVector(buffer.take(read))
  }
  println(s"Read ${data.length} bytes of data")

  println(Zstd.frameHeaderDesc.sizeBound)
  val res = Zstd.frame.decode(data.toBitVector)
  println(s"Result: $res")
  if (res.isFailure) println(s"Context: ${res.toEither.left.get.context}")
  else
    println(new String(res.require.value.blocks.head.asInstanceOf[Zstd.CompressedBlock].literals.data.toArray))
}

/*
manual decode of jsondata.zst

00000000  28 b5 2f fd 64 82 05 c5  0d 00 76 5c 57 24 30 6d  |(./.d.....v\W$0m|
00000010  f4 0c 63 28 2b 0b 69 ce  da 88 8c fc b1 c6 0a 27  |..c(+.i........'|
00000020  ce aa 6b 0e 7f 8d 34 61  e5 a6 5f 04 67 58 18 74  |..k...4a.._.gX.t|
00000030  8c 03 4c 00 4a 00 51 00  2e fd 3e 64 79 50 74 a2  |..L.J.Q...>dyPt.|
00000040  b3 de 3a 17 49 ab 44 f2  89 6b a9 64 b9 3f b1 9c  |..:.I.D..k.d.?..|
00000050  68 04 2b d5 53 92 4f 6b  7f 4e 94 95 8a 7a c7 74  |h.+.S.Ok.N...z.t|
00000060  c0 91 3f 32 b9 b6 2a cc  5c 74 5c 5c ab d1 6c 01  |..?2..*.\t\\..l.|
00000070  7b 01 57 33 8e 2c 7f aa  f4 fa db 88 2b 22 4f ec  |{.W3.,......+"O.|
00000080  9d 9e 2b 02 ae 57 e4 a7  a0 0c 2f d1 f9 13 92 cf  |..+..W..../.....|
00000090  86 bd f5 5a b7 e6 ff 78  3f 94 ae c1 7e 10 ce 68  |...Z...x?...~..h|
000000a0  ba 74 1c 67 c2 39 9f eb  d8 ce ba 0b b6 dc c2 1f  |.t.g.9..........|
000000b0  14 47 be ac 38 34 44 a6  ee 3b ac 37 26 d6 e5 54  |.G..84D..;.7&..T|
000000c0  a3 a7 58 a9 16 65 5f c2  0a fb 75 82 c5 6f 1d d9  |..X..e_...u..o..|
000000d0  26 48 54 35 f2 56 21 e0  52 2f 10 70 a9 75 06 c3  |&HT5.V!.R/.p.u..|
000000e0  44 20 97 a1 ea ad 05 0c  f5 d3 e4 8f 0a c1 3c a4  |D ............<.|
000000f0  3e 18 8e fc b8 be e0 88  40 2c ab b9 f6 ac 87 87  |>.......@,......|
00000100  35 0d f1 04 f9 74 52 59  19 dc 74 84 2e 3a ab f1  |5....tRY..t..:..|
00000110  ac 1f 78 89 16 48 3e 51  07 b5 95 95 d5 3c 30 00  |..x..H>Q.....<0.|
00000120  15 5b 4e 83 ac 71 8f 73  bd a0 e6 91 0f 83 2b 82  |.[N..q.s......+.|
00000130  2e a5 7b f8 cf 6a 50 fb  67 2b 9a f2 35 64 0d 49  |..{..jP.g+..5d.I|
00000140  68 b1 c1 02 c2 f2 ba b1  07 03 c6 01 6b f8 1a 63  |h...........k..c|
00000150  ce 98 9c 8b 45 af 1b 8a  da 6c 3f 27 f3 07 72 a7  |....E....l?'..r.|
00000160  1f 83 1e 8c 9e 74 b2 3d  80 2a 21 20 80 a2 81 b2  |.....t.=.*! ....|
00000170  ec 9b 94 e0 d2 72 eb 27  3b 75 88 45 69 70 48 fe  |.....r.';u.EipH.|
00000180  f5 b4 9c 95 27 9e 21 e2  38 5b a6 31 d9 e5 4b ac  |....'.!.8[.1..K.|
00000190  b1 1f 4f ad cf 7a ed 26  1c 41 91 5e 52 94 a2 78  |..O..z.&.A.^R..x|
000001a0  8a 00 03 da 5b d9 08 9e  cb e0 f0 36 44 8b d9 0a  |....[......6D...|
000001b0  0a 0d 63 63 23 0d 8b 23  e8 92 b3 c5 3e 1b e3 8b  |..cc#..#....>...|
000001c0  60 07 cb d3 df 47                                 |`....G|

24 <- header byte = 36 = huffman table size
30 6d f4 0c 63 28 2b 0b 69 ce  da 88 8c fc b1 c6 0a 27 ce aa 6b 0e 7f 8d 34 61  e5 a6 5f 04 67 58 18 74 8c 03 => huffman table?

30        6d        f4        0c        63
0011 0000 0110 1101 1111 0100 0000 1100 0110 0011

0000 <- low4bits = 0 accuracy = 1<<5 remaining = 33 read 6 or 5 bits 63-33 = 30, 0-29 use 5 bits
1 0011 <- 19, realcount = 18, sym 0, remaining 15, read 4 or 3 bits, 15-15 = 0, always use 4 bits
0 110 <- 6, realcount = 5, sym 1, remaining 10, read 4 or 3 bits, 15-10 = 5, 0-4 use 3 bits
011 <- 3, realcount = 2, sym 2, remaining 8, read 4 or 3 bits, 15-8=7, 0-6 use 3 bits
100 <- 4, realcount = 3, sym 3, remaining 5, read 3 or 2 bits, 7-5=2, 0-1 use 2 bits
11 0 <- 4, realcount = 3, sym 4, remaining 2, read 2 or 1 bits, 3-2=1, 0 use 1 bits
11 <- 2, realcount = 1, sym 5, remaining 1

63        0c
0110 0011 0000 1100 11 <- after reading FSE table

 0  0  1  4
 1  0  1  6
 2  0  1  8
 3  1  3  8
 4  4  4 16
 5  0  1 10
 6  0  1 12
 7  0  1 14
 8  2  4  0
 9  5  5  0
10  0  1 16
11  0  1 18
12  1  3 16
13  3  4 16
14  0  1 20
15  0  1 22
16  0  1 24
17  2  4 16
18  4  3  0
19  0  1 26
20  0  1 28
21  1  3 24
22  3  3  0
23  0  1 30
24  0  0  0
25  0  0  1
26  1  2  0
27  4  3  8
28  0  0  2
29  0  0  3
30  1  2  4
31  3  3  8

67 58 18 74 8c 03
0110 0111 0101 1000 0001 1000 0111 0100 1000 1100 0000 0011

0000 001 padding
11000 <- state1=24, sym 0, count 0, 0 bits, next state 0
11000 <- state2=24, sym 1, count 0, 0 bits, next state 0

state1 = 0 sym 2, count 0, 1 bits, offset 4
1 <- new state1 = 5
state1 = 0 sym 3, count 0, 1 bits, offset 4
1 <- new state2 = 5

111 0100 0001 1000 0101 1000 0110 011

Actual literals

                4c 00 4a 00 51 00  2e fd 3e 64 79 50 74 a2  |..L.J.Q...>dyPt.|
00000040  b3 de 3a 17 49 ab 44 f2  89 6b a9 64 b9 3f b1 9c  |..:.I.D..k.d.?..|
00000050  68 04 2b d5 53 92 4f 6b  7f 4e 94 95 8a 7a c7 74  |h.+.S.Ok.N...z.t|
00000060  c0 91 3f 32 b9 b6 2a cc  5c 74 5c 5c ab d1 6c 01  |..?2..*.\t\\..l.|
00000070  7b 01 57 33 8e 2c 7f aa  f4 fa db 88 2b 22 4f ec  |{.W3.,......+"O.|
00000080  9d 9e 2b 02 ae 57 e4 a7  a0 0c 2f d1 f9 13 92 cf  |..+..W..../.....|
00000090  86 bd f5 5a b7 e6 ff 78  3f 94 ae c1 7e 10 ce 68  |...Z...x?...~..h|
000000a0  ba 74 1c 67 c2 39 9f eb  d8 ce ba 0b b6 dc c2 1f  |.t.g.9..........|
000000b0  14 47 be ac 38 34 44 a6  ee 3b ac 37 26 d6 e5 54  |.G..84D..;.7&..T|
000000c0  a3 a7 58 a9 16 65 5f c2  0a fb 75 82 c5 6f 1d d9  |..X..e_...u..o..|
000000d0  26 48 54 35 f2 56 21 e0  52 2f 10 70 a9 75 06 c3  |&HT5.V!.R/.p.u..|
000000e0  44 20 97 a1 ea ad 05 0c  f5 d3 e4 8f 0a c1 3c a4  |D ............<.|
000000f0  3e 18 8e fc b8 be e0 88  40 2c ab b9 f6 ac 87 87  |>.......@,......|
00000100  35 0d f1 04 f9 74 52 59  19 dc 74 84 2e 3a ab f1  |5....tRY..t..:..|
00000110  ac 1f 78 89 16 48 3e 51  07 b5 95 95 d5 3c 30 00  |..x..H>Q.....<0.|
00000120  15 5b 4e 83 ac 71 8f 73  bd a0 e6 91 0f 83 2b 82  |.[N..q.s......+.|
00000130  2e a5 7b f8 cf 6a 50 fb  67 2b 9a f2 35 64 0d 49  |..{..jP.g+..5d.I|
00000140  68 b1 c1 02 c2 f2 ba b1  07 03 c6 01 6b f8 1a 63  |h...........k..c|
00000150  ce 98 9c 8b 45 af 1b 8a  da 6c 3f 27 f3 07 72 a7  |....E....l?'..r.|
00000160  1f 83 1e 8c 9e 74 b2 3d  80 2a


4c 00 4a 00 51 00 <- jump table

4c 00 <- stream1 len = 74
4a 00 <- stream2 len = 76
51 00 <- stream3 len = 81
stream4 = 349 - 231 ///* stream 1 - 3 */ - 6 /* jump table */ - 1 /* huffman header size */ - 36 /* huffman table */ = 75

Huffman table:

00000000  8 bits => '\n'
00000001  8 bits =>  ' '
00000010  8 bits =>  'B'
00000011  8 bits =>  'C'
00000100  8 bits =>  'D'
00000101  8 bits =>  'F'
00000110  8 bits =>  'G'
00000111  8 bits =>  'I'
00001000  8 bits =>  'J'
00001001  8 bits =>  'N'
00001010  8 bits =>  'O'
00001011  8 bits =>  'P'
00001100  8 bits =>  'S'
00001101  8 bits =>  '['
00001110  8 bits =>  ']'
00001111  8 bits =>  '_'
00010000  8 bits =>  'j'
00010001  8 bits =>  'k'
00010010  8 bits =>  'q'
00010011  8 bits =>  'u'
00010100  8 bits =>  'z'
00010101  8 bits =>  '{'
 0001011  7 bits =>  '-'
 0001100  7 bits =>  'M'
 0001101  7 bits =>  'T'
 0001110  7 bits =>  'g'
 0001111  7 bits =>  'h'
 0010000  7 bits =>  'm'
 0010001  7 bits =>  'p'
 0010010  7 bits =>  'v'
 0010011  7 bits =>  'y'
  001010  6 bits =>  ','
  001011  6 bits =>  '.'
  001100  6 bits =>  '/'
  001101  6 bits =>  '3'
  001110  6 bits =>  '6'
  001111  6 bits =>  '9'
  010000  6 bits =>  'b'
  010001  6 bits =>  'c'
  010010  6 bits =>  'l'
  010011  6 bits =>  'n'
  010100  6 bits =>  's'
  010101  6 bits =>  '}'
   01011  5 bits =>  '1'
   01100  5 bits =>  '2'
   01101  5 bits =>  '4'
   01110  5 bits =>  '5'
   01111  5 bits =>  '7'
   10000  5 bits =>  '8'
   10001  5 bits =>  ':'
   10010  5 bits =>  'd'
   10011  5 bits =>  'f'
   10100  5 bits =>  'i'
   10101  5 bits =>  'o'
   10110  5 bits =>  'r'
   10111  5 bits =>  't'
    1100  4 bits =>  '"'
    1101  4 bits =>  '0'
    1110  4 bits =>  'a'
    1111  4 bits =>  'e'


stream1 bytes: 2efd3e64795074a2b3de3a1749ab44f2896ba964b93fb19c68042bd553924f6b7f4e94958a7ac774c0913f32b9b62acc5c745c5cabd16c017b0157338e2c7faaf4fadb882b224fec9d9e2b02

02 2b 9e 9d

ec 4f 22 2b 88


0000 001   <- padding
0 0010 101 <- '{'
1 100      <- '"'
1 111      <- 'e'
0 1001 1   <- 'n'
10111      <- 't'
10110      <- 'r'
0 0100 11  <- 'y'
11 00      <- '"'
10 001     <- ':'
0 0010 101 <- '{'
1 100      <- '"'
0 1000

0010011

Sequences:
21 <- num sequences -> 33
20 = 0010 0000 <- offsets need custom table

80 a2 81 b2 <- start of offset fse table desc

ec




80 <- low4bits = 0: accuracy = 1<<5 = 2^5 = 32 -> 32 + 1 - 0 = 33, read 6 or 5 bits, 63-33=30, 0-29 use 5 bits, otherwise 6 bits
(11101 = 29, 011110 = 30 011111 = 31 100000 = 32

0 1000 <- count = 8, realcount = 7 (sym 0) read = 7 remaining = 33 - 7 = 26, read 5 or 4 bits, 31-26=0-4 use 4 bits
0 001  <- count = 1, realcount = 0 (sym 1) read = 7 remaining = 33 - 7 = 26, read 5 or 4 bits, 31-26=0-4 use 4 bits
01     <- repeat count = 1, realcount = 0 (sym 2)
001 1  <- count = 3, realcount = 2 (sym 3) read = 9 remaining = 33 - 9 = 24, read 5 or 4 bits, 31-24=0-6 use 4 bits
000 0  <- count = 0, realcount = -1 (sym 4) read = 10, remaining = 33 - 10 = 23, read 5 or 4 bits, 31-23=0-7 use 4 bits
010 1  <- count = 5, realcount = 4 (sym 5) read = 14, remaining = 33 - 14 = 19, read 5 or 4 bits, 31-19=0-11 use 4 bits
011 0  <- count = 6, realcount = 5 (sym 6) read = 19, remaining = 33 - 19 = 14, read 4 or 3 bits, 15-14=0-0 use 3 bits
100 1  <- count = 8  realcount = 7 (sym 7) read = 26, remaining = 33 - 26 = 7, read 3 or 2 bits, 7-7=0, always use 3 bits
10 1   <- count = 5  realcount = 4 (sym 8) read = 30, remaining = 33- 30 = 3, read 2 or 1 bits, 3-3=0, always use 2 bits
11     <- count = 3  realcount = 2 (sym 9) read = 33

Probs:

0 -> 7 -> next power is 8, width = 4 (2 bits), 8-7=1 counts double
1 -> 0
2 -> 0
3 -> 2 -> next power is 2, width = 16 (4 bits)
4 -> -1
5 -> 4 -> next power is 4, width = 8 (3 bits)
6 -> 5 -> next power is 8, width = 4 (2 bits), 8-5=3 counts double
7 -> 7 -> next power is 8, width = 4 (2 bits), 8-7=1 counts double
8 -> 4 -> next power is 4, width = 8 (3 bits)
9 -> 2 -> next power is 2, width = 16 (4 bits)

0 0 3 bits 24-31
1 3 4 bits 0-15
2 6 3 bits 8-15
3 7 3 bits 24-31
4 8 3 bits 0-7
5 0 2 bits 0-3
6 5 3 bits 0-7
7 6 3 bits 16-23
8 7 2 bits 0-3
9 9 4 bits 0-15
10 0 2 bits 4-7
11 6 3 bits 24-31
12 7 2 bits 4-7
13 8 3 bits 8-15
14 0 2 bits 8-11
15 5 3 bits 8-15
16 6 2 bits 0-3
17 7 2 bits 8-11
18 9 4 bits 16-31
19 0 2 bits 12-15
20 5 3 bits 16-23
21 7 2 bits 12-15
22 8 3 bits 16-23
23 0 2 bits 16-19
24 3 4 bits 16-32
25 6 2 bits 4-7
26 7 2 bits 16-19
27 8 3 bits 24-31
28 0 2 bits 20-23
29 5 3 bits 24-31
30 7 2 bits 20-23
31 4 5 bits

9b 94 e0 <- after table


 */

/*
manual decode of abc_times_100_with_middle_d.txt.zst

28 b5 2f fd 64 2d 00 85  00 00 20 61 62 63 64 03  |(./.d-.... abcd.|
00 00 02 61 61 a8 7b 90  72 43 fe 49 cd f4

28 b5 2f fd <- magic
64 <- frame header desc
(01: fcsFlag) (1: singleSegment) (00: reserved) (1: contentChecksum) (00: dictIdFlag)
2d 00 <- frame content size = 256 + 45 = 301
85 00 00 <- block header
(10000: blockSize) (10: blockType) (1: last)

20 <- literals header
(00100: regenSize) (0: sizeFormat) (00: literalsType)

61 62 63 64 <- raw literals

03 00 00 02 61 61 a8 7b 90  72 43

43        72        90        7b        a8
0100 0011 0111 0010 1001 0000 0111 1011 1010 1000

61        61        02        00        00        03
0110 0001 0110 0001 0000 0010 0000 0000 0000 0000 0000 0011

01 <- padding

000011 <- lit start state = 3 -> sym 3, 0 bits, 5 bits state
01110  <- offset state = 14   -> sym 2, 2 bits, 5 bits state
010100 <- match length state = 20 -> sym 43, 131 + 7 bits, 6 bits

10 <- offset extra bits -> offset = (1<<2) + 2 = 6 -> offset = 3
0000111 <- match len extra bits -> 131 + 7 = 138

sequence 1 = (3 lits, offset 3, 138 len)

 10111 -> lit state bits 23 -> new lit state = 23 -> sym 1, 0 bits, 4 bits state
010100 -> match state bits 20 -> new match state = 20 -> sym 43, 131 + 7 bits, 6 bits state
 00110 -> off state bits 6 -> new off state = 6 -> sym 7, 7 bits, 4 bits state

0001011 <- off extra bits -> offset_val = (1<<7) + 11 = 139 -> offset = 136
0000100 <- match extra bits 4 -> 131 + 4 = len 135

sequence 2 = (1 lit, offset 136, 135 len)

0000 <- lit -> 0 -> sym 0, 0 bits, 4 bits state
100000 <- match -> 32 -> sym 21, 0 bits, 6 bits state -> match len 24
0000  <- off -> 0 -> sym 0, 0 bits, 5 bits state

sequence 3 = (0 lit, offset 0 = Repeated_Offset2 = (seq 1) 3, 24 len)

0000 <- lit 0
000000 <- match 0
00001 <- off 1
1



fe 49 cd f4
 */

/*
Sequence(6,125,RepeatedOffset(1)),
Sequence(2,127,RepeatedOffset(1)),
Sequence(1,231,RepeatedOffset(1)),
Sequence(38,23,RepeatedOffset(1)),
Sequence(7,126,DirectOffset(2770)),
Sequence(1,127,RepeatedOffset(1)),
Sequence(1,230,RepeatedOffset(1)),
Sequence(40,23,RepeatedOffset(1)),
Sequence(4,127,RepeatedOffset(2)),
Sequence(2,127,RepeatedOffset(1)),
Sequence(1,230,RepeatedOffset(1)),
Sequence(40,23,RepeatedOffset(1)),
Sequence(6,125,RepeatedOffset(2)),
Sequence(2,127,RepeatedOffset(1)),
Sequence(1,230,RepeatedOffset(1)),
Sequence(40,23,RepeatedOffset(1)),
Sequence(5,126,DirectOffset(4432)),
Sequence(2,126,RepeatedOffset(1)),
Sequence(2,230,RepeatedOffset(1)),
Sequence(40,23,RepeatedOffset(1)),
Sequence(6,130,DirectOffset(6112)),
Sequence(8,126,RepeatedOffset(1)),
Sequence(2,230,RepeatedOffset(1)),
Sequence(40,23,RepeatedOffset(1)),
Sequence(6,47,RepeatedOffset(1)),
Sequence(1,48,RepeatedOffset(1)),
Sequence(1,29,RepeatedOffset(1)),
Sequence(8,113,RepeatedOffset(1)),
Sequence(1,12,RepeatedOffset(1)),
Sequence(2,230,RepeatedOffset(1)),
Sequence(40,23,RepeatedOffset(1)),
Sequence(6,125,RepeatedOffset(1)),
Sequence(2,126,RepeatedOffset(1)),
Sequence(2,53,RepeatedOffset(1)),
177 Sequence(1,176,RepeatedOffset(1)),
 62 Sequence(40,22,RepeatedOffset(1)),
 48 Sequence(7,41,RepeatedOffset(1)),
 30 Sequence(7,23,RepeatedOffset(1)),
 29 Sequence(0,29,DirectOffset(49)),
 23 Sequence(0,23,RepeatedOffset(1)),
 60 Sequence(4,56,RepeatedOffset(1)),
 44 Sequence(21,23,DirectOffset(562)),
 23 Sequence(0,23,DirectOffset(152)),
 12 Sequence(0,12,RepeatedOffset(1)),
232 Sequence(1,231,RepeatedOffset(1)),
 62 Sequence(39,23,RepeatedOffset(1)),
 52 Sequence(5,47,RepeatedOffset(1)),
 49 Sequence(2,47,RepeatedOffset(1)),
 28 Sequence(2,26,RepeatedOffset(1)),
124 Sequence(4,120,RepeatedOffset(1)),
 15 Sequence(2,13,RepeatedOffset(1)),
233 Sequence(1,232,RepeatedOffset(1)),
 60 Sequence(38,22,RepeatedOffset(1)),
132 Sequence(6,126,DirectOffset(561)),
136 Sequence(1,135,RepeatedOffset(1)),
232 Sequence(1,231,RepeatedOffset(1)),
 61 Sequence(39,22,RepeatedOffset(1)),
 54 Sequence(7,47,RepeatedOffset(2)),
 49 Sequence(1,48,RepeatedOffset(1)),
 27 Sequence(1,26,RepeatedOffset(1)),
125 Sequence(4,121,RepeatedOffset(1)),
 14 Sequence(1,13,RepeatedOffset(1)),
232 Sequence(1,231,RepeatedOffset(1)),
 61 Sequence(39,22,RepeatedOffset(1)),
 53 Sequence(7,46,RepeatedOffset(1)),
 49 Sequence(2,47,RepeatedOffset(1)),
 29 Sequence(2,27,RepeatedOffset(1)),
123 Sequence(3,120,RepeatedOffset(1)),
 15 Sequence(2,13,RepeatedOffset(1)),
233 Sequence(1,232,RepeatedOffset(1)),
 60 Sequence(38,22,RepeatedOffset(1)),
 54 Sequence(7,47,RepeatedOffset(1)),
 49 Sequence(1,48,RepeatedOffset(1)),
 27 Sequence(1,26,RepeatedOffset(1)),
 75 Sequence(4,71,RepeatedOffset(1)),
 49 Sequence(5,44,RepeatedOffset(2)),
 14 Sequence(1,13,RepeatedOffset(1))))))
 */ 