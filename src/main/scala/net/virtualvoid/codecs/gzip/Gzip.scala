package net.virtualvoid.codecs.gzip

import scala.annotation.tailrec
import net.virtualvoid.codecs.gzip.Gzip.BlockType.DynamicHuffman
import net.virtualvoid.codecs.gzip.Gzip.BlockType.FixedHuffman
import net.virtualvoid.codecs.gzip.Gzip.BlockType.Uncompressed
import scodec.Attempt.Failure
import scodec.Attempt.Successful
import scodec._
import scodec.bits._
import scodec.codecs._

import java.io.FileInputStream
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Gzip {
  case class GzipHeader(
      compressionMethod: Int,
      flags:             Int,
      mtime:             Long,
      xfl:               Int,
      os:                Int
  )

  lazy val gzipHeaderBase =
    constant(hex"1f8b") ~> (uint8 :: uint8 :: uint32 :: uint8 :: uint8).as[GzipHeader]

  lazy val gzipFile = gzipHeaderBase ~ deflateBlocks

  sealed trait BlockType
  object BlockType {
    case object Uncompressed extends BlockType
    case object FixedHuffman extends BlockType
    case object DynamicHuffman extends BlockType
  }

  case class DeflateBlockHeader(bfinal: Boolean, btype: BlockType)

  lazy val blockHeader = (bool :: blockType).as[DeflateBlockHeader]
  lazy val blockType =
    uint2R.xmap[BlockType]({
      case 0 => Uncompressed
      case 1 => FixedHuffman
      case 2 => DynamicHuffman
      case 3 => throw new IllegalArgumentException("Unsupported type 3")
    }, {
      case Uncompressed   => 0
      case FixedHuffman   => 1
      case DynamicHuffman => 2
    })

  lazy val deflateBlocks =
    (ReverseBitOrder ~> blockHeader)
      .consume[WithCodes] { header =>
        header.btype match {
          case FixedHuffman   => provide(new WithCodes(FixedLitLenCode, FixedDistanceCode))
          case DynamicHuffman => dynamicHuffmanTables
        }
      }(_ => ???)
      .consume(_.codes)(_ => ???)

  class WithCodes(litlenCode: HuffmanCode[LitLen], distanceCode: HuffmanCode[DistanceSymbol]) {
    val litlenCodec = huffman(litlenCode)
    val distanceCodec = huffman(distanceCode)

    // FIXME: should run until EndOfBlock
    lazy val codes: Codec[List[LiteralOrReference]] =
      new BuildListUntil(literalOrReference, _ == LiteralOrReference.EndOfBlock)

    lazy val literalOrReference: Codec[LiteralOrReference] =
      litlenCodec.consume[LiteralOrReference] {
        case Literal(b) => provide(LiteralOrReference.Literal(b))
        case Gzip.Length(_, extraBits, offset) =>
          (uintR(extraBits)
            .xmap[Int](_ + offset, _ => ???)
            :: distance).as[LiteralOrReference.Reference].upcast[LiteralOrReference]
        case EndOfBlock  => provide(LiteralOrReference.EndOfBlock)
        case r: Reserved => throw new IllegalStateException("Illegal use of reserved symbol")
      }(_ => ???)

    lazy val distance: Codec[Int] =
      huffman(distanceCode).consume {
        case DistanceSymbol(_, extraBits, offset) =>
          uintR(extraBits).xmap[Int](_ + offset, _ => ???)
      }(_ => ???)
  }

  sealed trait LiteralOrReference
  object LiteralOrReference {
    final case class Literal(value: Byte) extends LiteralOrReference
    final case class Reference(length: Int, distance: Int) extends LiteralOrReference
    final case object EndOfBlock extends LiteralOrReference
  }

  def huffman[T](code: HuffmanCode[T]): Codec[T] =
    Codec(_ => ???, { bits =>
      val (t, rest) = code.decode(bits)
      require(bits.size > rest.size, s"Code [$code] did make no progress after reading [$t]")
      Attempt.successful(DecodeResult(t, rest))
    })

  class BuildListUntil[T](inner: Codec[T], until: T => Boolean) extends TakeUntilCodec[T, List[T]](inner, until) {
    type U = ListBuffer[T]

    override def init: ListBuffer[T] = new ListBuffer[T]
    override def foldOne(result: ListBuffer[T], next: T): ListBuffer[T] = result += next
    override def finish(u: ListBuffer[T]): List[T] = u.result()
  }
  abstract class TakeUntilCodec[T, V](inner: Codec[T], until: T => Boolean) extends Codec[V] {
    type U
    def init: U
    def foldOne(result: U, next: T): U
    def finish(u: U): V

    override def decode(bits: BitVector): Attempt[DecodeResult[V]] = {
      @tailrec
      def decodeOne(intermediate: U, remaining: BitVector): Attempt[DecodeResult[V]] =
        inner.decode(remaining) match {
          case Successful(DecodeResult(t, rest)) =>
            require(remaining.sizeGreaterThan(rest.size), s"Didn't consume anything after reading [$t]")
            val nextIntermediate = foldOne(intermediate, t)
            //println(s"t: $t cond: ${until(t)}")
            if (until(t)) {
              val finalResult = finish(nextIntermediate)
              Successful(DecodeResult(finalResult, rest))
            } else
              decodeOne(nextIntermediate, rest)
          case f: Failure => f
        }

      decodeOne(init, bits)
    }
    override def encode(value: V): Attempt[BitVector] = ???
    override def sizeBound: SizeBound = SizeBound.unknown
  }

  abstract class FoldUntilCodec[T, V](inner: Codec[T]) extends Codec[V] {
    type U
    def init: U
    def foldOne(result: U, next: T): U
    def continue(intermediate: U): Boolean
    def finish(u: U): V

    override def decode(bits: BitVector): Attempt[DecodeResult[V]] = {
      @tailrec
      def decodeOne(intermediate: U, remaining: BitVector): Attempt[DecodeResult[V]] =
        inner.decode(remaining) match {
          case Successful(DecodeResult(t, rest)) =>
            val nextIntermediate = foldOne(intermediate, t)
            if (!continue(nextIntermediate)) {
              val finalResult = finish(nextIntermediate)
              Successful(DecodeResult(finalResult, rest))
            } else
              decodeOne(nextIntermediate, rest)
          case f: Failure => f
        }

      decodeOne(init, bits)
    }
    override def encode(value: V): Attempt[BitVector] = ???
    override def sizeBound: SizeBound = SizeBound.unknown
  }

  // TODO: convert to codec wrapper instead
  object ReverseBitOrder extends Codec[Unit] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Unit]] =
      Attempt.successful(DecodeResult((), bits.reverseBitOrder))
    override def encode(value: Unit): Attempt[BitVector] = ???
    override def sizeBound: SizeBound = SizeBound.unknown
  }

  def uint2R = uintR(2)
  def uintR(nBits: Int): Codec[Int] =
    new Codec[Int] {
      override def decode(bits: BitVector): Attempt[DecodeResult[Int]] = {
        val iBits = bits.take(nBits)
        val i = iBits.toIndexedSeq.foldRight(0) { (b, r) =>
          (r << 1) | (if (b) 1 else 0)
        }
        Attempt.successful(DecodeResult(i, bits.drop(nBits)))
      }
      override def encode(value: Int): Attempt[BitVector] = ???

      override def sizeBound: SizeBound = SizeBound.unknown
    }

  sealed trait LitLen {
    def symbolId: Int
  }
  object LitLen {
    implicit def hasOrdering: Ordering[LitLen] = Ordering.by(_.symbolId)
  }
  final case class Literal(b: Byte) extends LitLen {
    override def symbolId: Int = b & 0xff
  }
  case object EndOfBlock extends LitLen {
    override def symbolId: Int = 256
  }
  final case class Length(symbolId: Int, extraBits: Int, offset: Int) extends LitLen
  final case class Reserved(symbolId: Int) extends LitLen
  val LitLens: Seq[LitLen] =
    (0 to 255).map(b => Literal(b.toByte)) ++
      Seq(EndOfBlock) ++
      Seq(
        Length(257, 0, 3),
        Length(258, 0, 4),
        Length(259, 0, 5),
        Length(260, 0, 6),
        Length(261, 0, 7),
        Length(262, 0, 8),
        Length(263, 0, 9),
        Length(264, 0, 10),
        Length(265, 1, 11),
        Length(266, 1, 13),
        Length(267, 1, 15),
        Length(268, 1, 17),
        Length(269, 2, 19),
        Length(270, 2, 23),
        Length(271, 2, 27),
        Length(272, 2, 31),
        Length(273, 3, 35),
        Length(274, 3, 43),
        Length(275, 3, 51),
        Length(276, 3, 59),
        Length(277, 4, 67),
        Length(278, 4, 83),
        Length(279, 4, 99),
        Length(280, 4, 115),
        Length(281, 5, 131),
        Length(282, 5, 163),
        Length(283, 5, 195),
        Length(284, 5, 227),
        Length(285, 0, 258),
        Reserved(286),
        Reserved(287)
      )

  def idToLitLen(id: Int): LitLen = LitLens(id)

  val FixedLitLenCodeLengths: Seq[(LitLen, Int)] =
    ((0 to 143).map(_ -> 8) ++
      (144 to 255).map(_ -> 9) ++
      (256 to 279).map(_ -> 7) ++
      (280 to 287).map(_ -> 8))
      .map {
        case (id, length) => idToLitLen(id) -> length
      }
  val FixedLitLenCode = HuffmanCode(FixedLitLenCodeLengths)

  final case class DistanceSymbol(symbolId: Int, extraBits: Int, offset: Int)
  object DistanceSymbol {
    implicit def hasOrdering: Ordering[DistanceSymbol] = Ordering.by(_.symbolId)
  }

  /*
           Extra           Extra               Extra
    Code Bits Dist  Code Bits   Dist     Code Bits Distance
    ---- ---- ----  ---- ----  ------    ---- ---- --------
      0   0    1     10   4     33-48    20    9   1025-1536
      1   0    2     11   4     49-64    21    9   1537-2048
      2   0    3     12   5     65-96    22   10   2049-3072
      3   0    4     13   5     97-128   23   10   3073-4096
      4   1   5,6    14   6    129-192   24   11   4097-6144
      5   1   7,8    15   6    193-256   25   11   6145-8192
      6   2   9-12   16   7    257-384   26   12  8193-12288
      7   2  13-16   17   7    385-512   27   12 12289-16384
      8   3  17-24   18   8    513-768   28   13 16385-24576
      9   3  25-32   19   8   769-1024   29   13 24577-32768
   */

  val DistanceSymbols =
    Seq(
      DistanceSymbol(0, 0, 1),
      DistanceSymbol(1, 0, 2),
      DistanceSymbol(2, 0, 3),
      DistanceSymbol(3, 0, 4),
      DistanceSymbol(4, 1, 5),
      DistanceSymbol(5, 1, 7),
      DistanceSymbol(6, 2, 9),
      DistanceSymbol(7, 2, 13),
      DistanceSymbol(8, 3, 17),
      DistanceSymbol(9, 3, 25),
      DistanceSymbol(10, 4, 33),
      DistanceSymbol(11, 4, 49),
      DistanceSymbol(12, 5, 65),
      DistanceSymbol(13, 5, 97),
      DistanceSymbol(14, 6, 129),
      DistanceSymbol(15, 6, 193),
      DistanceSymbol(16, 7, 257),
      DistanceSymbol(17, 7, 385),
      DistanceSymbol(18, 8, 513),
      DistanceSymbol(19, 8, 769),
      DistanceSymbol(20, 9, 1025),
      DistanceSymbol(21, 9, 1537),
      DistanceSymbol(22, 10, 2049),
      DistanceSymbol(23, 10, 3073),
      DistanceSymbol(24, 11, 4097),
      DistanceSymbol(25, 11, 6145),
      DistanceSymbol(26, 12, 8193),
      DistanceSymbol(27, 12, 12289),
      DistanceSymbol(28, 13, 16385),
      DistanceSymbol(29, 13, 24577)
    )
  val FixedDistanceCode = HuffmanCode(DistanceSymbols.map(_ -> 5))

  sealed trait CodeLengthSymbol {
    def symbolId: Int
  }
  trait WithExtraBits {
    def extraBits: Int
    def offset: Int
  }
  object CodeLengthSymbol {
    final case class LiteralLength(length: Int) extends CodeLengthSymbol {
      override def symbolId: Int = length
    }
    case object CopyPrevious extends CodeLengthSymbol with WithExtraBits {
      override def symbolId: Int = 16
      override def extraBits: Int = 2
      override def offset: Int = 3
    }
    case class RepeatLength0(symbolId: Int, extraBits: Int, offset: Int) extends CodeLengthSymbol with WithExtraBits
    implicit def hasOrdering: Ordering[CodeLengthSymbol] = Ordering.by(_.symbolId)
  }
  val CodeLengthSymbols =
    (0 to 15).map(CodeLengthSymbol.LiteralLength) ++
      Seq(
        CodeLengthSymbol.CopyPrevious,
        CodeLengthSymbol.RepeatLength0(17, 3, 3),
        CodeLengthSymbol.RepeatLength0(18, 7, 11)
      )
  val CodeLengthSymbolWeirdOrder = Seq(16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15)
  val CodeLengthSymbolsInWeirdOrder =
    CodeLengthSymbolWeirdOrder.map(CodeLengthSymbols.apply)

  import shapeless.{ ::, HNil }
  def dynamicHuffmanTables: Codec[WithCodes] =
    (uintR(5) :: uintR(5) :: uintR(4)).consume[WithCodes] {
      case hlen :: hdist :: hclen :: HNil =>
        //val lengths = listOfN(provide(hclen + 4), uintR(3))
        codeLengths(hclen + 4, hlen + hdist + 258).xmap({ lens =>
          val litLenLens = lens.take(hlen + 257)
          val distanceLens = lens.drop(hlen + 257) // remaining: hdist + 1

          val litLenCode = HuffmanCode(LitLens.zip(litLenLens))
          println(s"hlen: $hlen hdist: $hdist")
          println(s"LitLenLens: $litLenLens")
          println(s"zipped: ${LitLens.zip(litLenLens).filter(_._2 != 0)}")
          println(litLenCode)

          val distCode = HuffmanCode(DistanceSymbols.zip(distanceLens))
          println(s"distanceLens: $distanceLens")

          println(distCode)

          new WithCodes(litLenCode, distCode)
        }, _ => ???)
    }(_ => ???)

  def codeLengthHuffman(numLengths: Int): Codec[HuffmanCode[CodeLengthSymbol]] =
    listOfN(provide(numLengths), uintR(3))
      .xmap({ lens =>
        val lengthMappings = CodeLengthSymbolsInWeirdOrder.zip(lens)
        println(s"Length mappings: $lengthMappings")
        HuffmanCode(lengthMappings)
      }, _ => ???)

  def codeLengths(numLengths: Int, numLengthsToRead: Int): Codec[Seq[Int]] =
    codeLengthHuffman(numLengths).consume { lenHuff =>

      new FoldUntilCodec[Seq[Int], Seq[Int]](oneLengthItem(lenHuff)) {
        override type U = ListBuffer[Int]
        override def init: U = new ListBuffer[Int]
        override def foldOne(result: ListBuffer[Int], next: Seq[Int]): ListBuffer[Int] = {
          if (next.head >= 0)
            result ++= next
          else
            result ++= Seq.fill(-next.head)(result.last) // copy previous element 3-6 times
        }
        override def continue(intermediate: ListBuffer[Int]): Boolean = intermediate.size < numLengthsToRead
        override def finish(u: ListBuffer[Int]): Seq[Int] = u.result()
      }
    }(_ => ???)

  def oneLengthItem(lenHuff: HuffmanCode[CodeLengthSymbol]): Codec[Seq[Int]] = {
    huffman(lenHuff).consume[Seq[Int]]({
      case l @ CodeLengthSymbol.LiteralLength(length) =>
        println(s"Got $l")
        provide(Seq(length))
      case r0 @ CodeLengthSymbol.RepeatLength0(_, extraBits, offset) =>
        println(s"Got $r0")
        uintR(extraBits).xmap({ extra =>
          println(s"Got extra $extra")
          Seq.fill(extra + offset)(0)
        }, _ => ???)
      case CodeLengthSymbol.CopyPrevious =>
        println(s"Got CopyPrevious")
        uintR(2).xmap({ extra =>
          println(s"Got extra $extra")
          Seq(-extra - 3) // HACK: use negative number for copy previous
        }, _ => ???)
    })(_ => ???)
  }
}

object Test extends App {
  import Gzip._

  val data = {
    //val fis = new FileInputStream("abc_times_100_with_middle_d.txt.zst")
    val fis = new FileInputStream("jsondata30.json.9.gz")
    val buffer = new Array[Byte](10000)
    val read = fis.read(buffer)
    require(read > 0)
    ByteVector(buffer.take(read))
  }
  println(s"Read ${data.length} bytes of data")

  println(Gzip.gzipFile.decode(data.bits))
}
trait HuffmanCode[T] {
  def decode(bits: BitVector): (T, BitVector)
}
object HuffmanCode {
  def apply[T: Ordering](bitLengths: Seq[(T, Int)]): HuffmanCode[T] = {
    val grouped = bitLengths.filterNot(_._2 == 0).groupBy(_._2)
    //println(grouped)
    val lengthHisto = grouped.view.mapValues(_.size).toMap.withDefaultValue(0)
    println(lengthHisto)
    val maxLength = lengthHisto.keys.max
    val firsts = (0 to maxLength).scanLeft((0, 0, 0))((state, cur) =>
      ((state._1 + state._2) << 1, lengthHisto(cur), cur)
    ).filter(_._3 > 0).map {
      case (value, _, length) => length -> value
    }.toMap

    val mapping =
      grouped.map {
        case (length, group) =>
          val first = firsts(length)
          length ->
            group.sortBy(_._1).zipWithIndex.map {
              case ((t, _), i) => first + i -> t
            }
      }.toSeq.sortBy(_._1)

    new HuffmanCode[T] {
      override def decode(bits: BitVector): (T, BitVector) = {
        @tailrec
        def find(remainingGroups: Seq[(Int, Seq[(Int, T)])]): (T, BitVector) = {
          val (length, els) = remainingGroups.head
          val value = bits.take(length).toIndexedSeq.foldLeft(0)((r, b) => (r << 1) | (if (b) 1 else 0))
          els.find(_._1 == value) match {
            case Some((_, t)) =>
              //println(s"Found [${t}] dropping $length bits, remaining size: ${bits.drop(length).size} bits")
              t -> bits.drop(length)
            case None => find(remainingGroups.tail)
          }

        }

        //println(s"Trying to find code in ${bits.take(20).toBin}")
        find(mapping)
      }

      override def toString: String =
        mapping.flatMap(m => m._2.map(e => (e._1, e._2, m._1))).map(e => f"${bin(e._1, e._3)}%-15s ${e._3}%2d => ${e._2}").mkString("\n")

      private def bin(code: Int, len: Int): String = {
        val res = code.toBinaryString
        "0" * (len - res.size) + res
      }

    }
  }
}