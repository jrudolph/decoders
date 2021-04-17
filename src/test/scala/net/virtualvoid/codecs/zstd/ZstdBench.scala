package net.virtualvoid.codecs.zstd

import scodec.bits.ByteVector

import java.io.FileInputStream

object ZstdBench extends App {
  val fileName = "jsondata5000.json.19.zst"
  val data = {
    val fis = new FileInputStream(fileName)
    val buffer = new Array[Byte](1000000)
    val read = fis.read(buffer)
    require(read > 0)
    require(read < buffer.size)
    ByteVector(buffer.take(read))
  }
  println(s"Read ${data.length} bytes of data")

  println(Zstd.frameHeaderDesc.sizeBound)
  val res = Zstd.frame.decode(data.toBitVector)
  if (res.isFailure) println(s"Context: ${res.toEither.left.get.context}")

  val bits = data.bits

  def bench[T](what: String, amount: Double, unit: String)(body: => T): T = {
    val start = System.nanoTime()
    val res = body
    val end = System.nanoTime()
    val lastedMillis = (end - start) / 1000000
    val ratePerSecond = amount / lastedMillis * 1000
    println(f"$what took $lastedMillis ms per $amount%5.2f $unit: $ratePerSecond%5.2f $unit/s")
    res
  }

  val resBytes = res.require.value.decode.size

  println("Benchmarking")
  (0 until 10).foreach { _ =>
    val times = 10
    bench("decoding", resBytes * times / 1000000, "MB") {
      (0 until times).foreach(_ => Zstd.frame.decode(bits).require.map(_.decode))
    }
  }
}
