object Say {

  def ordinals = Seq("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
  def enteens = Seq("ten", "eleven", "twelve") ++ ordinals.drop(3).map(o => if (o.endsWith("t")) o + "een" else o + "teen")
  def enties = Seq("", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninenty")
  def orders = List("", "thousand", "million", "billion")

  def sayBelowHundred(number: Int): String = {
    if (number < 10) {
      ordinals(number)
    } else if (number < 20) {
      enteens(number - 10)
    } else if (number % 10 == 0) {
      enties(number / 10)
    } else {
      s"${enties(number / 10)}-${ordinals(number % 10)}"
    }
  }

  def sayBelowThousand(number: Int) = {
    val hundredsPart = if (number < 100) None else {
      Some(s"${sayBelowHundred(number / 100)} hundred")
    }
    hundredsPart.fold(sayBelowHundred(number % 100)) { part =>
      if (number % 100 == 0) part else s"$part ${sayBelowHundred(number % 100)}"
    }
  }

  def inEnglish(number: Long): Option[String] = {
    def _say(rest: Long, ordersTail: List[String], acc: List[String]): String = {
      if (rest == 0) {
        if (acc.isEmpty) "zero" else acc.mkString(" ")
      } else {
        if (rest % 1000L == 0) {
          _say(rest / 1000L, ordersTail.tail, acc)
        } else {
          val countPart = sayBelowThousand((rest % 1000L).toInt)
          val order = ordersTail.head
          val part = if (order.isEmpty) countPart else s"$countPart $order"
          _say(rest / 1000L, ordersTail.tail, part :: acc)
        }
      }
    }

    if (number >= 0 && number <= 999999999999L) {
      Some(_say(number, orders, Nil))
    } else {
      None
    }
  }
}
