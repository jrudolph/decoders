package net.virtualvoid.codecs

import scodec.Codec

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
}
