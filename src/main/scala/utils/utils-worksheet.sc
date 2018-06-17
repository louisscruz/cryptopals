def time[R](block: => R): R = {
  val t0 = System.currentTimeMillis()
  val result = block    // call-by-name
  val t1 = System.currentTimeMillis()
  println("Elapsed time: " + (t1 - t0) + "ms")
  result
}

time { hexToBase64("F") }

time { bufferXor("abcdef123", 'C') }

time { deceipherXor("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736") }