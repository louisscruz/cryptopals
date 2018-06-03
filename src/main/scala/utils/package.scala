import scala.collection.parallel.immutable.ParVector

package object utils {
  val base64Chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  val base64Mappings = for {
    c1 <- base64Chars
    c2 <- base64Chars
  } yield s"$c1$c2"

  def hexToBase64(input: String): String = {
    // Pad non-even inputs with a preceding zero.
    if (input.length % 2 != 0) return hexToBase64(s"0$input")

    def processString(input: String): String = {
      if (input.length == 3) return base64Mappings(Integer.parseInt(input, 16))

      val firstValue = Integer.parseInt(input.slice(0, 1), 16)
      val secondValue = Integer.parseInt(input.slice(1, 2), 16)

      return base64Mappings(((firstValue << 2) * 64) + ((secondValue & 3) << 4))
    }

    val groupings = new ParVector(input.grouped(3)[Vector])

    val baseString = groupings.map(processString).mkString

    val paddingAmount = (4 - (baseString.length % 4))

    if (paddingAmount > 2) return baseString

    val paddings = "=" * paddingAmount

    s"$baseString$paddings"
  }
}
