package net.virtualvoid.codecs

object Utils {
  def char(idx: Int): String =
    idx match {
      case 0xa         => "'\\n'"
      case 0xd         => "'\\r'"
      case x if x < 32 => " .  "
      case x           => s" '${x.toChar.toString}'"
    }
}
