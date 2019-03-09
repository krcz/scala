object AtbashCipher {
  val alphabetSpan = 'z' - 'a'

  def transform(phrase: String): String = {
    phrase.map(c => if (c.isLetter) ('a' + (alphabetSpan - (c - 'a'))).toChar else c)
  }

  def encode(phrase: String): String = {
    transform(phrase.toLowerCase.replaceAll("[^\\w]", "")).grouped(5).mkString(" ")
  }

  def decode(phrase: String): String = {
    transform(phrase.replaceAll("[^\\w]", ""))
  }
}
