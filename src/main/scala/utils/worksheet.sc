import utils._

def time[R](block: => R): R = {
  val t0 = System.currentTimeMillis()
  val result = block    // call-by-name
  val t1 = System.currentTimeMillis()
  println("Elapsed time: " + (t1 - t0) + "ms")
  result
}

time { hexToBase64("F") }

// I currently get ambiguous reference errors in the worksheet due to overloading.
//time { bufferXor("abcdef123": String, 'C': Char) }

time { deceipherXor("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736") }

def rotate(s: String): String = {
  s.tail + s.head
}

def rotations(s: String, n: Int): List[String] = {
  if(n == s.length) Nil else rotate(s) :: rotations(rotate(s), n + 1)
}

val lines = rotations("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736", 0)

time { detectSingleCharXor(lines) }

time { repeatingKeyXor("Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal", "ICE") }