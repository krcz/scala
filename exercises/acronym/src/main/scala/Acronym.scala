object Acronym {
  def abbreviate(phrase: String): String = {
    phrase
      .toLowerCase
      .replace("-", " ")
      .filter(c => c.isLetter || c == ' ')
      .split(" ")
      .filter(_.nonEmpty)
      .map(part => part.head.toUpper)
      .mkString("")
  }
}
