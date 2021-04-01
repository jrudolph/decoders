package net.virtualvoid.codecs.gzip

import scodec.{ Attempt, Codec, DecodeResult, SizeBound }
import scodec.bits.{ BitVector, ByteVector, HexStringSyntax }
import scodec.codecs._
import shapeless._

import java.io.FileInputStream

object Zstd {
  case class Frame(
      header: FrameHeader,
      block:  Block // FIXME: multiple blocks
  )

  lazy val frame: Codec[Frame] =
    (constant(hex"28B52FFD") ~> frameHeader :: block).as[Frame]

  case class FrameHeader(
      headerDesc:       FrameHeaderDesc,
      frameContentSize: Long
  )

  lazy val frameHeader: Codec[FrameHeader] = frameHeaderDesc.consume { desc =>
    val frameContentSizeLength = desc.frameContentSizeFlag match {
      case 0 if desc.singleSegment => 1
      case 0                       => 0
      case 1                       => 2
      case 2                       => 4
      case 3                       => 8
    }
    val fcsBase = if (frameContentSizeLength == 2) 256 else 0

    println(desc)
    //println(s"fcs_length: $frameContentSizeLength")
    require(desc.singleSegment) // can skip window desc

    (provide(desc) :: ulongL(frameContentSizeLength * 8).xmap[Long](_ + fcsBase, _ - fcsBase)).as[FrameHeader]
  }(_ => ???)

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
  case class Literals(
      literalsType:    Int,
      regeneratedSize: Int,
      compressedSize:  Int,
      data:            ByteVector
  )
  sealed trait Offset
  case class DirectOffset(offset: Int) extends Offset
  case class RepeatedOffset(idx: Int) extends Offset

  case class Sequence(literalLen: Int, matchLen: Int, offset: Offset)
  case class Sequences(
      header:    SequenceSectionHeader,
      sequences: Seq[Sequence]
  )

  lazy val block: Codec[Block] =
    blockHeader.consume { header =>
      require(header.blockType == 2) // compressed block
      variableSizeBytes(provide(header.blockSize), compressedBlock(header).widen[Block](b => b, _ => ???))
    }(_ => ???)

  case class BlockHeader(
      lastBlock: Boolean,
      blockType: Int,
      blockSize: Int
  )
  lazy val blockHeader: Codec[BlockHeader] =
    uint24L.xmap[BlockHeader]({ i =>
      val last = (i & 1) != 0
      val blockType = (i & 6) >> 1
      val blockSize = i >> 3
      BlockHeader(last, blockType, blockSize)
    }, _ => ???)

  def compressedBlock(header: BlockHeader): Codec[CompressedBlock] = {
    require(header.blockType == 2) // compressed block
    (provide(header) :: literals :: sequences).as[CompressedBlock]
  }

  lazy val literals: Codec[Literals] = {
    // FIXME: hard to decode because of weird bit order
    (uint4 :: uint2 :: uint2).consume {
      case lenBits :: sizeFormat :: tpe :: HNil =>
        //require(tpe == 0, s"Literal type == $tpe but only 0 supported") // raw, FIXME: support more
        tpe match {
          case 0 => // raw literals
            val len = sizeFormat match {
              case 0 => lenBits << 1
              case 1 => lenBits << 1 + 1
            }
            (provide(tpe) :: provide(len) :: provide(len) :: bytes(len)).as[Literals]
          case 2 => // Compressed_Literals_Block
            sizeFormat match {
              case 1 =>
                uint16L.consume { len =>
                  val regenSize = ((len & 0x3f) << 4) + lenBits
                  val compSize = len >> 6
                  //println(s"lenBits: ${lenBits.toHexString} len: ${len.toHexString} regenSize: $regenSize compSize: $compSize ${(len & 0x3f).toHexString}")
                  (provide(tpe) :: provide(regenSize) :: provide(compSize) :: bytes(compSize)).as[Literals]
                }(_ => ???)
              case 2 =>
                uint24L.consume { len =>
                  val regenSize = (len & 0x3ff) << 4 + lenBits
                  val compSize = len >> 10
                  (provide(tpe) :: provide(regenSize) :: provide(compSize) :: bytes(compSize)).as[Literals]
                }(_ => ???)
            }

        }
    }(_ => ???)
  }

  lazy val sequences: Codec[Sequences] =
    sequenceSectionHeader.consume { header =>
      val litLenTable = header.litLengthTable.toTable
      val matchLenTable = header.matchLengthTable.toTable
      val offsetTable = header.offsetTable.toTable

      println("Litlen")
      println(litLenTable)
      println("MatchLen")
      println(matchLenTable)
      println("Offset")
      println(offsetTable)

      reversed {
        withReversedBits {
          peek(uint8).consume { first =>
            val padding = java.lang.Integer.numberOfLeadingZeros(first) - 24 + 1

            (ignore(padding) ~> uint(header.litLengthTable.accuracyLog) :: uint(header.offsetTable.accuracyLog) :: uint(header.matchLengthTable.accuracyLog)).consume {
              case litLenState :: offsetState :: matchLenState :: HNil =>
                println(litLenState, offsetState, matchLenState)

                def nextSequence(remaining: Int, litLenState: Int, offsetState: Int, matchLenState: Int, current: Vector[Sequence]): Codec[Seq[Sequence]] =
                  if (remaining == 0) provide(current)
                  else {
                    // offset, matchlen, lit
                    val offsetEntry = offsetTable.entries(offsetState)

                    println(s"Offset entry: $offsetEntry")
                    val offsetCode = offsetEntry.symbol

                    def readExtra(bits: Int): Codec[Int] = if (bits == 0) provide(0) else uint(bits)

                    readExtra(offsetCode).consume { extra =>
                      val offsetValue = (1 << offsetCode) + extra
                      val offset =
                        if (offsetValue > 3) DirectOffset(offsetValue - 3)
                        else RepeatedOffset(offsetValue)

                      println(s"Offset: $offset")

                      val matchLenEntry = matchLenTable.entries(matchLenState)
                      val matchLenCode = MatchLenCodes(matchLenEntry.symbol)
                      readExtra(matchLenCode.extraBits).consume { extra =>
                        val matchLen = matchLenCode.baseline + extra
                        println(s"MatchLen: $matchLen")

                        val litLenEntry = litLenTable.entries(litLenState)
                        val litLenCode = LitLenCodes(litLenEntry.symbol)

                        readExtra(litLenCode.extraBits).consume { extra =>
                          val litLen = litLenCode.baseline + extra
                          println(s"LitLen: $litLen")

                          val allSeqs: Vector[Sequence] = current :+ Sequence(litLen, matchLen, offset)
                          if (remaining > 1)
                            // state update: `Literals_Length_State` is updated, followed by `Match_Length_State`, and then `Offset_State`
                            (uint(litLenEntry.nbBits) :: uint(matchLenEntry.nbBits) :: uint(offsetEntry.nbBits)).consume {
                              case ll :: ml :: o :: HNil =>
                                nextSequence(remaining - 1, ll + litLenEntry.offset, o + offsetEntry.offset, ml + matchLenEntry.offset, allSeqs)
                            }(_ => ???)
                          else provide(allSeqs: Seq[Sequence])

                        }(_ => ???)
                      }(_ => ???)

                    }(_ => ???)
                  }

                (provide(header) :: nextSequence(header.numberOfSequences, litLenState, offsetState, matchLenState, Vector.empty)).as[Sequences]
            }(_ => ???)
          }(_ => ???)
        }
      }
    }(_ => ???)

  case class SequenceSectionHeader(
      numberOfSequences: Int,
      litLengthMode:     Int,
      offsetMode:        Int,
      matchLengthMode:   Int,
      litLengthTable:    FSETableSpec,
      offsetTable:       FSETableSpec,
      matchLengthTable:  FSETableSpec
  )

  lazy val sequenceSectionHeader: Codec[SequenceSectionHeader] =
    (numberOfSequences :: uint2 :: uint2 :: uint2 :: ("reserved sequence section modes" | constant(BitVector.bits(Iterable(false, false))))).flatConcat {
      case _ :: lMode :: oMode :: mLMode :: _ :: HNil =>
        def tableFor(mode: Int, defaultSpec: FSETableSpec): Codec[FSETableSpec] = {
          mode match {
            case 0 => provide(defaultSpec)
            case 2 => fseTableSpec
          }
        }

        tableFor(lMode, DefaultLitLenTable) :: tableFor(oMode, DefaultOffsetTable) :: tableFor(mLMode, DefaultMatchLenTable)
    }
      .as[SequenceSectionHeader]

  lazy val numberOfSequences: Codec[Int] =
    uint8.consume[Int] {
      case 0            => provide(0)
      case x if x < 128 => provide(x)
      // FIXME: support remaining modes
    }(_ => ???)

  case class FSETableSpec(
      accuracyLog: Int,
      histogram:   Seq[Int]
  ) {
    def toTable: FSETable[Int] = {
      val size = 1 << accuracyLog
      val mask = size - 1
      val skip = (size >> 1) + (size >> 3) + 3
      val minMinus1 = size - histogram.count(_ == -1)
      val buffer = new Array[FSETableEntry[Int]](size)

      var lastIdx = size - 1
      histogram.zipWithIndex.filter(_._1 == -1).foreach {
        case (_ /* -1 */ , symbol) =>
          require(buffer(lastIdx) == null)
          buffer(lastIdx) = FSETableEntry(symbol, accuracyLog, 0)
          lastIdx -= 1
      }

      var pos = 0
      def allocate(entry: FSETableEntry[Int]): Unit = {
        buffer(pos) = entry
        do {
          pos = (pos + skip) & mask
        } while (pos >= minMinus1)
      }

      histogram.zipWithIndex.foreach {
        case (count, symbol) =>
          if (count > 0) {
            val e = FSETableEntry(symbol, -1, -1) // bits and offset to be filled later
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
              case ((_, idx), num) =>
                val newEntry =
                  if (num < double) // double entry
                    FSETableEntry(symbol, bits + 1, doubleStartOffset + 2 * width * num)
                  else // single entry
                    FSETableEntry(symbol, bits, width * (num - double))

                buffer(idx) = newEntry
            }
          }
      }

      FSETable(buffer.toIndexedSeq)
    }
  }
  case class FSETableEntry[T](
      symbol: T,
      nbBits: Int,
      offset: Int
  )
  case class FSETable[T](entries: IndexedSeq[FSETableEntry[T]]) {
    override def toString: String =
      entries.zipWithIndex.map {
        case (null, state) => f"$state%4d null"
        case (FSETableEntry(symbol, nbBits, offset), state) =>
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

  case class MatchLenCode(baseline: Int, extraBits: Int)
  lazy val MatchLenCodes: IndexedSeq[MatchLenCode] =
    (Vector.fill(31)(0) ++ Vector(1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
      .scanLeft(MatchLenCode(3, 0)) { (code, nextBits) =>
        MatchLenCode(code.baseline + (1 << code.extraBits), nextBits)
      }

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
  case class LitLenCode(baseline: Int, extraBits: Int)
  lazy val LitLenCodes: IndexedSeq[LitLenCode] =
    (Vector.fill(15)(0) ++ Vector(1, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
      .scanLeft(LitLenCode(0, 0)) { (code, nextBits) =>
        LitLenCode(code.baseline + (1 << code.extraBits), nextBits)
      }

  /**
   * Reverses bits while running inner codec
   */
  def withReversedBits[T](inner: Codec[T]): Codec[T] =
    new Codec[T] {
      override def decode(bits: BitVector): Attempt[DecodeResult[T]] =
        inner.decode(bits.reverseBitOrder).map(_.mapRemainder(_.reverseBitOrder))
      override def encode(value: T): Attempt[BitVector] =
        inner.encode(value).map(_.reverseBitOrder)
      override def sizeBound: SizeBound = inner.sizeBound
    }

  def reversed[T](inner: Codec[T]): Codec[T] =
    new Codec[T] {
      override def decode(bits: BitVector): Attempt[DecodeResult[T]] =
        inner.decode(bits.reverse).map(_.mapRemainder(_.reverse))
      override def encode(value: T): Attempt[BitVector] =
        inner.encode(value).map(_.reverse)
      override def sizeBound: SizeBound = inner.sizeBound
    }

  /**
   * Reads `bits` from a little endian bit stream and interprets them reversed
   */
  def uintLEBits(bits: Int): Codec[Int] = limitedSizeBits(bits, withReversedBits(uintL(bits)))

  lazy val fseTableSpec: Codec[FSETableSpec] = withReversedBits {

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
          peek(uintLEBits(bits + 1)).consume { bitsRead =>
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
          }(_ => ???)
        }

      def afterZero: Codec[FSETableSpec] =
        uintLEBits(2).consume { num =>
          val repeatCounts = Vector.fill(num)(0)
          val nextState = copy(counts = counts ++ repeatCounts)
          if (num == 3) // next repeat
            nextState.afterZero
          else
            nextState.next
        }(_ => ???)
    }

    uintLEBits(4).consume { log0 =>
      val log = log0 + 5
      val states = 1 << log

      ReadState(log, states + 1, Vector.empty).next
    }(_ => ???)
  }

  def peekPrintNextBits(num: Int, tag: String): Codec[Unit] =
    peek(bits(num)).xmap({ bv => println(s"$tag: ${bv.toBin}") }, _ => ???)

}

object ZstdTest extends App {
  val data = {
    //val fis = new FileInputStream("abc_times_100_with_middle_d.txt.zst")
    val fis = new FileInputStream("jsondata.json.zst")
    val buffer = new Array[Byte](10000)
    val read = fis.read(buffer)
    require(read > 0)
    ByteVector(buffer.take(read))
  }
  println(s"Read ${data.length} bytes of data")

  println(Zstd.frameHeaderDesc.sizeBound)
  val res = Zstd.frame.decode(data.toBitVector)
  println(s"Result: $res")
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