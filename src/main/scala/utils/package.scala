import scala.collection.parallel.immutable.ParVector

package object utils {
  val base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  val base64Mappings = for {
    c1 <- base64Chars
    c2 <- base64Chars
  } yield s"$c1$c2"
  val englishLikelihood = Map[Char, Int](
    ' ' -> 40,
    'e' -> 12,
    't' -> 8,
    'a' -> 8,
    'o' -> 7,
    'i' -> 7,
    'n' -> 7,
    's' -> 6,
    'h' -> 6,
    'r' -> 6
  )

  def hexToBase64(input: String): String = {
    // Pad non-even inputs with a preceding zero.
    if (input.length % 2 != 0) return hexToBase64(s"0$input")

    def processString(input: String): String = {
      if (input.length == 3) return base64Mappings(Integer.parseInt(input, 16))

      val firstValue = Integer.parseInt(input.slice(0, 1), 16)
      val secondValue = Integer.parseInt(input.slice(1, 2), 16)

      return base64Mappings(((firstValue << 2) * 64) + ((secondValue & 3) << 4))
    }

    val groupings = new ParVector(input.grouped(3).to[Vector])

    val baseString = groupings.map(processString).mkString

    val paddingAmount = (4 - (baseString.length % 4))

    if (paddingAmount > 2) return baseString

    val paddings = "=" * paddingAmount

    s"$baseString$paddings"
  }

  val hexChars = "0123456789abcdef"
  val xorMappings = (for {
    a <- hexChars
  } yield {
    val aInt = Integer.parseInt(a.toString, 16)
    val subMap = (for { b <- hexChars } yield {
      val bInt = Integer.parseInt(b.toString, 16)
      val value = (aInt ^ bInt).toHexString
      (b -> value)
    }).toMap

    (a -> subMap)
  }).toMap

  def bufferXor(a: String, b: String) = {
    if (a.length == b.length) {
      (for {
        (first, second) <- a.toList.zip(b.toList)
      } yield xorMappings(first)(second)).mkString
    } else {
      throw new Error("argument lengths did not match")
    }
  }

  def bufferXor(a: String, b: Char) = {
    a.grouped(2).map(el => (Integer.parseInt(el, 16) ^ b.toInt).toChar).mkString
  }

  def englishAnalysisScore(input: String): Float = {
    input.map(el => englishLikelihood.get(Character.toLowerCase(el)) match {
      case Some(x) => x
      case None => 0
    }).sum / input.length.asInstanceOf[Float]
  }

  def deceipherXor(input: String) = {
    (0 to 127).par.map(el => bufferXor(input, el.toChar)).maxBy(englishAnalysisScore)
  }

  def detectSingleCharXor(input: List[String]) = {
    input.par.map(line => {
      val responses = (0 to 127).par.map(el => bufferXor(line, el.toChar))
      responses.foldLeft((responses.head, englishAnalysisScore(responses.head)))((a, b) => {
        val nextVal = (b, englishAnalysisScore(b))
        if (a._2 > nextVal._2) a else nextVal
      })
    }).maxBy(_._2)._1
  }
}
