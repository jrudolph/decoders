package net.virtualvoid.codecs.zstd

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scodec.{ Attempt, Codec, DecodeResult }
import scodec.bits.{ BitVector, ByteVector, HexStringSyntax }
import Zstd._
import org.scalatest.matchers.Matcher

class ZStdSpec extends AnyFreeSpec with Matchers {
  "ZStd should" - {
    "decode" - {
      "FrameHeader" - {
        "with single segment flag set" in {
          hex"28 b5 2f fd 64 2d 00" must
            fullyDecodeTo(Zstd.frameHeader, FrameHeader(FrameHeaderDesc(1, true, true, 0), 301, 301))
        }
        "with zero size" in pending
        "with size type 2" in pending
        "with size type 3" in pending
        "without single segment flag but window size" in pending
      }
      "BlockHeader" in {
        hex"85 00 00" must fullyDecodeTo(Zstd.blockHeader, BlockHeader(true, 2, 16))
        hex"c5 0d 00" must fullyDecodeTo(Zstd.blockHeader, BlockHeader(true, 2, 440))
      }
      "Literals" - {
        "raw literals with short size" in {
          hex"20 61 62 63 64" must fullyDecodeTo(Zstd.literals, Literals(LiteralSpec(0, 4, 4, 1), ByteVector.encodeAscii("abcd").right.get))
        }
      }
      "Sequences" - {
        "using predefined tables" in {
          hex"03 00 00 02 61 61 a8 7b 90 72 43" must fullyDecodeTo(
            Zstd.sequences,
            Sequences(
              SequenceSectionHeader(3, 0, 0, 0, DefaultLitLenTable, DefaultOffsetTable, DefaultMatchLenTable),
              Seq(Sequence(3, 138, DirectOffset(3)), Sequence(1, 135, DirectOffset(136)), Sequence(0, 24, RepeatedOffset(1)))
            )
          )
        }

      }
      "FSETableSpec" - {
        "custom offset table" in {
          hex"80 a2 81 b2 ec" must fullyDecodeTo(
            Zstd.fseTableSpec,
            FSETableSpec(5, Seq(7, 0, 0, 2, -1, 4, 5, 7, 4, 2))
          )
        }
        "custom huffman length table" in {
          hex"30 6d f4" must fullyDecodeTo(
            Zstd.fseTableSpec,
            FSETableSpec(5, Seq(18, 5, 2, 3, 3, 1))
          )
        }
      }
      "HuffmanTableSpec" - {
        "custom table" in {
          hex"30 6d f4 0c 63 28 2b 0b 69 ce da 88 8c fc b1 c6 0a 27 ce aa 6b 0e 7f 8d 34 61 e5 a6 5f 04 67 58 18 74 8c 03" must fullyDecodeTo(
            Zstd.huffmanSpec,
            HuffmanSpec(8, Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 2, 3, 3, 5, 4, 4, 3, 4, 4, 3, 4, 4, 3, 4, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 0, 2, 1, 1, 1, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 5, 3, 3, 4, 5, 4, 2, 2, 4, 1, 1, 3, 2, 3, 4, 2, 1, 4, 3, 4, 1, 2, 0, 0, 2, 1, 1, 0, 3))
          )
        }
      }
    }
  }

  def fullyDecodeTo[T](codec: Codec[T], expected: T): Matcher[ByteVector] =
    (left: ByteVector) => be(Attempt.Successful(DecodeResult(expected, BitVector.empty)))(codec.decode(left.bits))
}
