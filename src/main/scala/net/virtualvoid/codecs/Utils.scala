package net.virtualvoid.codecs

import scodec.bits.{ BitVector, ByteVector }
import scodec.codecs.{ bits, bytes, peek }
import scodec.{ Attempt, Codec, DecodeResult, SizeBound }

import scala.annotation.tailrec
import scala.collection.immutable.VectorBuilder

object Utils {
  def char(idx: Int): String =
    idx match {
      case 0xa                      => "'\\n'"
      case 0xd                      => "'\\r'"
      case x if x < 32 || x >= 0x80 => "  . "
      case x                        => s" '${x.toChar.toString}'"
    }

  implicit class WithFlatMapD[T](val codec: Codec[T]) extends AnyVal {
    def flatMapD[U](f: T => Codec[U]): Codec[U] =
      codec.consume(f)(_ => throw new NotImplementedError("Only decoding supported"))

    def mapD[U](f: T => U): Codec[U] = codec.xmap(f, _ => throw new NotImplementedError("Only decoding supported"))
  }

  /**
   * Starting from an initial state, `nextCodec` provides a codec that reads the next value and optionally a new state.
   * The resulting codec collects all values until this function returns no new state.
   */
  def collectWithState[T, S](initialState: S)(nextCodec: S => Codec[(T, Option[S])]): Codec[Seq[T]] =
    new Codec[Seq[T]] {
      override def decode(bits: BitVector): Attempt[DecodeResult[Seq[T]]] = {
        @tailrec
        def next(bits: BitVector, state: S, collected: VectorBuilder[T]): Attempt[DecodeResult[Seq[T]]] =
          nextCodec(state).decode(bits) match {
            case Attempt.Successful(DecodeResult((nextEle, nextStateO), bits)) =>
              collected += nextEle
              nextStateO match {
                case Some(nextState) => next(bits, nextState, collected)
                case None            => Attempt.Successful(DecodeResult(collected.result(), bits))
              }

            case f: Attempt.Failure => f
          }

        next(bits, initialState, new VectorBuilder[T])
      }

      override def sizeBound: SizeBound = SizeBound.unknown
      override def encode(value: Seq[T]): Attempt[BitVector] = ???
    }

  /**
   * Reverses bits while running inner codec
   */
  def withReversedBits[T](inner: Codec[T]): Codec[T] = mapInputBits(_.reverseBitOrder)(inner)
  def reversed[T](inner: Codec[T]): Codec[T] = mapInputBits(_.reverse)(inner)
  def compact[T](inner: Codec[T]): Codec[T] = mapInputBits(_.compact)(inner)
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

  def peekPrintNextBits(num: Int, tag: String): Codec[Unit] =
    peek(bits(num)).mapD { bv => trace(s"$tag: ${bv.toBin}") }

  def peekPrintNextBytes(num: Int, tag: String): Codec[Unit] =
    peek(bytes(num)).mapD { bv => trace(s"$tag: ${bv.toHex}") }

  def peekRemainingBytes(tag: String): Codec[Unit] =
    peek(bytes.mapD { bv => trace(s"$tag: ${bv.size} bytes ${bv.toHex}") })

  private val TRACE = false
  def trace(o: Any): Unit = if (TRACE) trace(o.toString)
}
