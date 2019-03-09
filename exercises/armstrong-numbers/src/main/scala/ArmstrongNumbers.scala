object ArmstrongNumbers {
  def ipow(exp: Int)(v: Int): Int = {
    List.fill(exp)(v).foldLeft(1)(_ * _)
  }

  // returns reversed list of number digits
  def rdigits(v: Int, acc: List[Int] = Nil): List[Int] = {
    if (v == 0) {
      acc
    } else {
      rdigits(v / 10, v % 10 :: acc)
    }
  }

  def isArmstrongNumber(number: Int): Boolean = {
    val digits = rdigits(number)
    digits.map(ipow(digits.size)).sum == number
  }
}
