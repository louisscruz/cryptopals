import utils._

def time[R](block: => R): R = {
  val t0 = System.currentTimeMillis()
  val result = block    // call-by-name
  val t1 = System.currentTimeMillis()
  println("Elapsed time: " + (t1 - t0) + "ms")
  result
}

time { hexToBase64("F") }

time { bufferXor("abcdef123", "123abcdef") }