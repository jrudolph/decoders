package net.virtualvoid.codecs.zstd

import scodec.bits.ByteVector

import java.io.FileInputStream

object ZstdTest extends App {
  val data = {
    val fis = new FileInputStream("abc_times_100.txt.zst")
    //val fis = new FileInputStream("repeated.txt.19.zst")
    //val fis = new FileInputStream("jsondata30.json.19.zst")
    val buffer = new Array[Byte](100000)
    val read = fis.read(buffer)
    require(read > 0)
    ByteVector(buffer.take(read))
  }
  println(s"Read ${data.length} bytes of data")

  println(Zstd.frameHeaderDesc.sizeBound)
  val res = Zstd.frame.decode(data.toBitVector)
  println(s"Result: $res")
  if (res.isFailure) println(s"Context: ${res.toEither.left.get.context}")
  else {
    val lits = new String(res.require.value.blocks.head.asInstanceOf[Zstd.CompressedBlock].literals.data.toArray)
    println(s"Lits: [$lits]")
    val decoded = res.require.value.decode
    println(s"fully decoded: [${decoded.decodeUtf8.right.get}]")
  }
}