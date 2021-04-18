package net.virtualvoid.codecs.zstd

import net.virtualvoid.codecs.Utils
import Utils._
import scodec.{ Attempt, Codec, DecodeResult, SizeBound }
import scodec.bits.{ BitVector, ByteVector, HexStringSyntax }
import scodec.codecs._
import shapeless._

import scala.annotation.tailrec

object Zstd {
  /**
   * The decoding table uses an accuracy log of 6 bits (64 states).
   *
   * ```
   * short literalsLength_defaultDistribution[36] =
   *         { 4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1,
   *           2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 1, 1,
   *          -1,-1,-1,-1 };
   * ```
   */
  val DefaultLitLenTable =
    FSETableSpec(6, Seq(4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 1, 1, -1, -1, -1, -1))

  /**
   * The decoding table uses an accuracy log of 6 bits (64 states).
   * ```
   * short matchLengths_defaultDistribution[53] =
   *         { 1, 4, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,
   *           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
   *           1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,-1,-1,
   *          -1,-1,-1,-1,-1 };
   * ```
   */
  val DefaultMatchLenTable =
    FSETableSpec(6, Seq(1, 4, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1))

  /**
   * The decoding table uses an accuracy log of 5 bits (32 states),
   * and supports a maximum `N` value of 28, allowing offset values up to 536,870,908 .
   *
   * If any sequence in the compressed block requires a larger offset than this,
   * it's not possible to use the default distribution to represent it.
   * ```
   * short offsetCodes_defaultDistribution[29] =
   *         { 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,
   *           1, 1, 1, 1, 1, 1, 1, 1,-1,-1,-1,-1,-1 };
   * ```
   */
  val DefaultOffsetTable =
    FSETableSpec(5, Seq(1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, -1, -1, -1, -1, -1))

  /**
   * | `Literals_Length_Code` |         0-15           |
   * | ---------------------- | ---------------------- |
   * | length                 | `Literals_Length_Code` |
   * | `Number_of_Bits`       |          0             |
   *
   * | `Literals_Length_Code` |  16  |  17  |  18  |  19  |  20  |  21  |  22  |  23  |
   * | ---------------------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
   * | `Baseline`             |  16  |  18  |  20  |  22  |  24  |  28  |  32  |  40  |
   * | `Number_of_Bits`       |   1  |   1  |   1  |   1  |   2  |   2  |   3  |   3  |
   *
   * | `Literals_Length_Code` |  24  |  25  |  26  |  27  |  28  |  29  |  30  |  31  |
   * | ---------------------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
   * | `Baseline`             |  48  |  64  |  128 |  256 |  512 | 1024 | 2048 | 4096 |
   * | `Number_of_Bits`       |   4  |   6  |   7  |   8  |   9  |  10  |  11  |  12  |
   *
   * | `Literals_Length_Code` |  32  |  33  |  34  |  35  |
   * | ---------------------- | ---- | ---- | ---- | ---- |
   * | `Baseline`             | 8192 |16384 |32768 |65536 |
   * | `Number_of_Bits`       |  13  |  14  |  15  |  16  |
   */
  val LitLenCodeTable: IndexedSeq[Codec[Int]] =
    intCodeTableWithExtraBits(0, Vector.fill(16)(0) ++ Vector(1, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))

  /**
   * | `Match_Length_Code` |         0-31            |
   * | ------------------- | ----------------------- |
   * | value               | `Match_Length_Code` + 3 |
   * | `Number_of_Bits`    |          0              |
   *
   * | `Match_Length_Code` |  32  |  33  |  34  |  35  |  36  |  37  |  38  |  39  |
   * | ------------------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
   * | `Baseline`          |  35  |  37  |  39  |  41  |  43  |  47  |  51  |  59  |
   * | `Number_of_Bits`    |   1  |   1  |   1  |   1  |   2  |   2  |   3  |   3  |
   *
   * | `Match_Length_Code` |  40  |  41  |  42  |  43  |  44  |  45  |  46  |  47  |
   * | ------------------- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
   * | `Baseline`          |  67  |  83  |  99  |  131 |  259 |  515 | 1027 | 2051 |
   * | `Number_of_Bits`    |   4  |   4  |   5  |   7  |   8  |   9  |  10  |  11  |
   *
   * | `Match_Length_Code` |  48  |  49  |  50  |  51  |  52  |
   * | ------------------- | ---- | ---- | ---- | ---- | ---- |
   * | `Baseline`          | 4099 | 8195 |16387 |32771 |65539 |
   * | `Number_of_Bits`    |  12  |  13  |  14  |  15  |  16  |
   */
  val MatchLenCodeTable: IndexedSeq[Codec[Int]] =
    intCodeTableWithExtraBits(3, Vector.fill(32)(0) ++ Vector(1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))

  /**
   * Offset codes are values ranging from `0` to `N`.
   *
   * An offset code is also the number of additional bits to read in __little-endian__ fashion,
   * and can be translated into an `Offset_Value` using the following formulas :
   *
   * ```
   * Offset_Value = (1 << offsetCode) + readNBits(offsetCode);
   * if (Offset_Value > 3) offset = Offset_Value - 3;
   * ```
   *
   * `Offset_Value` from 1 to 3 are special : they define "repeat codes".
   */
  val OffsetCodeTable: IndexedSeq[Codec[Offset]] =
    (0 to 31).map { offsetCode =>
      readExtra(offsetCode).mapD { offsetExtra =>
        val offsetValue = (1 << offsetCode) + offsetExtra

        if (offsetValue > 3) DirectOffset(offsetValue - 3)
        else RepeatedOffset(offsetValue - 1)
      }
    }

  /**
   * For the first block, the starting offset history is populated with following values :
   * `Repeated_Offset1`=1, `Repeated_Offset2`=4, `Repeated_Offset3`=8,
   * unless a dictionary is used, in which case they come from the dictionary.
   */
  val InitialOffsetHistory = Seq(1, 4, 8)

  case class Frame(
      header:   FrameHeader,
      blocks:   Seq[Block],
      checksum: Option[Long]
  ) {
    /** Do the actual LZ decoding. It keeps the full output in memory for simplicity */
    def decode: ByteVector = {
      @tailrec
      def processNextBlock(decoded: ByteVector, offsetHistory: Seq[Int], blocks: Seq[Block]): ByteVector = blocks match {
        case Nil => decoded
        case CompressedBlock(_, Literals(_, _, lits), Sequences(_, seqs)) +: rest =>
          val (newDecoded, lastOffs) = processSequence(decoded, lits, offsetHistory, seqs)
          processNextBlock(newDecoded, lastOffs, rest)
      }
      @tailrec
      def processSequence(decoded: ByteVector, literals: ByteVector, offsetHistory: Seq[Int], sequences: Seq[Sequence]): (ByteVector, Seq[Int]) = sequences match {
        case Nil => (decoded ++ literals, offsetHistory) // append remaining literals at the end
        case Sequence(litLen, matchLen, offset) +: rest =>
          val dec0 = decoded ++ literals.take(litLen)
          val (off, newOffs) = offset match {
            case DirectOffset(offset) =>
              (offset, offset +: offsetHistory.dropRight(1))
            case RepeatedOffset(idx) if litLen > 0 =>
              val thisOff = offsetHistory(idx)
              val newOffs = idx match {
                case 0 => offsetHistory
                case 1 => Seq(offsetHistory(1), offsetHistory(0), offsetHistory(2))
                case 2 => Seq(offsetHistory(2), offsetHistory(0), offsetHistory(1))
              }
              (thisOff, newOffs)

            case RepeatedOffset(idx) =>
              val thisOff = idx match {
                case 0 => offsetHistory(1)
                case 1 => offsetHistory(2)
                case 2 => offsetHistory(0) - 1
              }

              val newOffs = idx match {
                case 0 => Seq(offsetHistory(1), offsetHistory(0), offsetHistory(2))
                case 1 => Seq(offsetHistory(2), offsetHistory(0), offsetHistory(1))
                case 2 => Seq(thisOff, offsetHistory(1), offsetHistory(2))
              }
              (thisOff, newOffs)
          }
          /** Resolves overlapping matches (probably not in the fastest way) */
          def includingMatch(window: ByteVector, offset: Int, matchLen: Int): ByteVector = {
            val thisMatchLen = math.min(matchLen, offset)
            val remainingMatchLen = matchLen - thisMatchLen
            val after = window ++ window.slice(window.size - off, window.size - off + matchLen)
            if (remainingMatchLen > 0) includingMatch(after, offset, remainingMatchLen)
            else after
          }

          val all = includingMatch(dec0, off, matchLen)
          processSequence(all, literals.drop(litLen), newOffs, rest)
      }

      val result = processNextBlock(ByteVector.empty, InitialOffsetHistory, blocks)
      require(header.frameContentSize == -1 || result.size == header.frameContentSize, s"Result size ${result.size} != frame content size ${header.frameContentSize}")
      result
    }
  }

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

    //trace(desc)
    //trace(s"fcs_length: $frameContentSizeLength")
    val frameContentSize = {
      if (frameContentSizeLength > 0)
        ulongL(frameContentSizeLength * 8).xmap[Long](_ + fcsBase, _ - fcsBase)
      else
        provide(-1L)
    }
    lazy val windowSize: Codec[Long] =
      uint8.mapD[Long](_.toLong
      /** FIXME: needs to be split into exponent / mantissa */
      )

    if (desc.singleSegment)
      (provide(desc) :: frameContentSize).mapD[FrameHeader] {
        case desc :: fcs :: HNil => FrameHeader(desc, fcs, fcs)
      }
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
    (provide(header) :: literals(blockState) :: sequences(blockState)).as[CompressedBlock]
  }

  def literals(blockState: BlockState): Codec[Literals] =
    literalSpec.flatPrepend { spec =>
      spec.literalsType match {
        case 0 => provide(None: Option[HuffmanSpec]) :: bytes(spec.compressedSize)
        case 2 => variableSizeBytes(provide(spec.compressedSize), compressedLiterals(spec))
        case 3 => variableSizeBytes(provide(spec.compressedSize), decodeLiterals(spec, blockState.huffmanSpec.get))
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
          case 2 | 3 => // Compressed_Literals_Block or Treeless_Literals_Block
            def sizes(sizeFormat: Int): Codec[Int :: Int :: Int :: HNil] =
              sizeFormat match {
                case 0 | 1 =>
                  val numStreams = if (sizeFormat == 0) 1 else 4
                  uint16L.flatMapD { len =>
                    val regenSize = ((len & 0x3f) << 4) + lenBits
                    val compSize = len >> 6
                    //trace(s"lenBits: ${lenBits.toHexString} len: ${len.toHexString} regenSize: $regenSize compSize: $compSize ${(len & 0x3f).toHexString}")
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
      trace(s"totalWeights: $totalWeights nextPowerOf2: $nextPowerOf2 remaining: $remainingBins last weight: $lastWeight")

      new HuffmanSpec(maxNumberOfBits, collectedWeights :+ lastWeight)
    }
  }
  case class HuffmanEntry(symbol: Int, weight: Int, numberOfBits: Int, code: Int, shiftedCode: Int, mask: Int) {
    def toString(maxNumberOfBits: Int): String = f"${binStringWithLeftZeros(code, numberOfBits)}%10s ${binStringWithLeftZeros(shiftedCode, maxNumberOfBits)} ${mask.toBinaryString} ${numberOfBits}%2d bits => ${Utils.char(symbol)}"
  }

  case class HuffmanTable(maxNumberOfBits: Int, entries: Seq[HuffmanEntry]) {
    override def toString: String =
      entries.map(_.toString(maxNumberOfBits)).mkString("\n")

    /** A simple lookup table for all values of length `maxNumberOfBits` */
    private val table: Array[HuffmanEntry] = {
      val res = new Array[HuffmanEntry](1 << maxNumberOfBits)
      entries.foreach { e =>
        val numEntries = 1 << (maxNumberOfBits - e.numberOfBits)
        (0 until numEntries).foreach { offset =>
          res(e.shiftedCode + offset) = e
        }
      }
      res
    }

    /** code should contain `maxNumberOfBits` of new data */
    def read(code: Int): HuffmanEntry = table(code)
  }
  private def binStringWithLeftZeros(num: Int, numberOfBits: Int): String = {
    val str = num.toBinaryString
    "0" * (numberOfBits - str.size) + str
  }

  def ifEnoughDataAvailable[T](inner: Codec[T]): Codec[Option[T]] =
    lookahead(inner.mapD[Unit](_ => ())).flatMapD[Option[T]] { canRead =>
      conditional(canRead, inner)
    }

  lazy val huffmanSpec: Codec[HuffmanSpec] =
    fseTableSpec.flatMapD { tableSpec =>
      val table = tableSpec.toTable((0 to 20).map(provide))
      trace(s"Huffman table weights FSE: $table")
      def nextValues(state1: Int, state2: Int, current: Seq[Int]): Codec[HuffmanSpec] = {
        val v1Entry = table.entries(state1)
        val v2Entry = table.entries(state2)
        trace(s"Read entries ${v1Entry.symbol} ${v2Entry.symbol}")

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
          compact {
            (padding ~> uint(tableSpec.accuracyLog) :: uint(tableSpec.accuracyLog)).flatMapD {
              case state1 :: state2 :: HNil =>
                trace(s"Start states state1: $state1 state2: $state2")
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
      decodeLiterals(spec, huffmanSpec)
    }

  def decodeLiterals(spec: LiteralSpec, huffmanSpec: HuffmanSpec): Codec[Option[HuffmanSpec] :: ByteVector :: HNil] = {
    trace(s"Lit Spec: $spec Huffman Spec: $huffmanSpec")
    trace(huffmanSpec.toTable)

    provide(Some(huffmanSpec): Option[HuffmanSpec]) :: {
      if (spec.numStreams == 4)
        (uint16L :: uint16L :: uint16L).flatMapD {
          case stream1 :: stream2 :: stream3 :: HNil =>
            val numEls = (spec.regeneratedSize + 3) / 4
            val remaining = spec.regeneratedSize - 3 * numEls

            (variableSizeBytes(provide(stream1), decodeLiteralStream(huffmanSpec, numEls)) ::
              variableSizeBytes(provide(stream2), decodeLiteralStream(huffmanSpec, numEls)) ::
              variableSizeBytes(provide(stream3), decodeLiteralStream(huffmanSpec, numEls)) ::
              decodeLiteralStream(huffmanSpec, remaining)).mapD[ByteVector] {
                case s1 :: s2 :: s3 :: s4 :: HNil => s1 ++ s2 ++ s3 ++ s4
              }
        }
      else
        decodeLiteralStream(huffmanSpec, spec.regeneratedSize)
    }
  }

  def decodeLiteralStream(huffmanSpec: HuffmanSpec, numElements: Int): Codec[ByteVector] = {
    val table = huffmanSpec.toTable
    new Codec[ByteVector] {
      override def sizeBound: SizeBound = SizeBound.unknown
      override def encode(value: ByteVector): Attempt[BitVector] = ???

      override def decode(bits: BitVector): Attempt[DecodeResult[ByteVector]] = {
        val result = new Array[Byte](numElements)
        val inputBuf = bits.bytes

        @tailrec
        def decodeNext(readPadding: Boolean, inputBufPos: Long, readBuf: Long, bits: Int, outputBufPos: Int): Attempt[DecodeResult[ByteVector]] =
          if (outputBufPos < result.size) {
            var iPos = inputBufPos
            var buf = readBuf
            var bs = bits
            // refill bits as necessary
            if (bs < 16) // only fill when almost empty (could also be maxNumberOfBits if we know the max good enough
              while (bs <= 56 && iPos >= 0) {
                val nextByte = (inputBuf(iPos) & 0xff).toLong
                buf = buf | (nextByte << (56 - bs))
                iPos -= 1
                bs += 8
                //println(s"filling up with nextByte ${nextByte.toHexString}")
                //println(s"Buf is now filled with $bs bits: ${buf.toBinaryString}")
              }
            //println(s"Buf is now filled with $bs bits: ${buf.toBinaryString}")
            if (readPadding) {
              val p = java.lang.Long.numberOfLeadingZeros(buf)
              require(p < 8)
              buf = buf << (p + 1)
              bs -= p + 1
              //println(s"after padding Buf is now filled with $bs bits: ${buf.toBinaryString}")
            }

            val i = buf >>> (64 - table.maxNumberOfBits)
            val e = table.read(i.toInt)
            //println(s"read code ${i.toBinaryString} entry ${e.toString(table.maxNumberOfBits)}")
            buf = buf << e.numberOfBits
            bs -= e.numberOfBits

            result(outputBufPos) = e.symbol.toByte
            decodeNext(false, iPos, buf, bs, outputBufPos + 1)
          } else Attempt.Successful(DecodeResult(ByteVector(result), BitVector.empty))

        decodeNext(true, inputBuf.size - 1, readBuf = 0, bits = 0, outputBufPos = 0)
      }
    }
  }

  def decodeLiteralStream2(huffmanSpec: HuffmanSpec, numElements: Int): Codec[ByteVector] = {
    val table = huffmanSpec.toTable

    reversed {
      withReversedBits {
        appendInput(ByteVector(0)) {
          compact {
            def readOne: Codec[Int] =
              peek(uint(table.maxNumberOfBits)).flatMapD { v =>
                //trace(s"Read ${v.toBinaryString}")
                val entry = table.read(v)
                //trace(s"${char(entry.symbol)} Found entry: $entry")

                ignore(entry.numberOfBits) ~> provide(entry.symbol)
              }

            padding ~> vectorOfN(provide(numElements), readOne).mapD[ByteVector](bs => ByteVector(bs: _*))
          }
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

      trace("Litlen")
      trace(header.litLengthTable.histogram)
      trace(litLenTable)
      trace("MatchLen")
      trace(header.matchLengthTable.histogram)
      trace(matchLenTable)
      trace("Offset")
      trace(header.offsetTable.histogram)
      trace(offsetTable)

      reversed {
        withReversedBits {
          compact {
            (padding ~> litLenTable.decodeInitialState :: offsetTable.decodeInitialState :: matchLenTable.decodeInitialState).flatMapD {
              case litLenState :: offsetState :: matchLenState :: HNil =>
                trace(litLenState, offsetState, matchLenState)

                val seqs = Utils.collectWithState(header.numberOfSequences :: litLenState :: matchLenState :: offsetState :: HNil) {
                  case remaining :: litLenState :: matchLenState :: offsetState :: HNil =>
                    //trace(s"Remaining: $remaining ll: $litLenState ml: $matchLenState o: $offsetState")
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
        trace(s"num seqs $num modes $lMode $oMode $mLMode")

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

            trace(s"Symbol: $symbol count: $count nextPower: $nextPower width: $width bits: $bits double: $double single: ${count - double}")

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

  private def intCodeTableWithExtraBits(base: Int, extraBits: Vector[Int]): IndexedSeq[Codec[Int]] = {
    case class Code(baseline: Int, extraBits: Int)

    val first = extraBits.head
    extraBits.tail
      .scanLeft(Code(base, first)) { (code, nextBits) =>
        Code(code.baseline + (1 << code.extraBits), nextBits)
      }
      .map(c => readExtra(c.extraBits).mapD(_ + c.baseline))
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
              //trace(s"accuracyLog: $accuracyLog remaining: $remaining threshold: $threshold max: $max bitsRead: $bitsRead value: $value bitsUsed: $bitsUsed realCount: $realCount")
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
}
