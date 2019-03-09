import scala.util.Try

object AllYourBase {

  /** Computes pair (value / b, value % b)
    *
    *  Both value and value / b are represented as lists of digits with base of a
    *  while value % b is plain integer */
  def divModB(a: Int, digits: List[Int], b: Int): (List[Int], Int) = {
    val (mod, reverseDiv) = digits.foldLeft((0, List.empty[Int])) { (acc, digit) =>
      val (rem, reverseDiv) = acc
      val nominator = rem * a + digit
      (nominator % b, (nominator / b) :: reverseDiv)
    }

    (reverseDiv.reverse.dropWhile(_ == 0), mod)
  }

  def realRebase(a: Int, digits: List[Int], b: Int, acc: List[Int] = Nil): List[Int] = {
    if (digits.isEmpty) {
      if(acc.nonEmpty) acc else List(0)
    } else {
      val (divb, modb) = divModB(a, digits, b)
      realRebase(a, divb, b, modb :: acc)
    }
  }

  def rebase(a: Int, digits: List[Int], b: Int): Option[List[Int]] = {
    Try {
      assert (a >= 2)
      assert (b >= 2)
      assert (digits.forall(d => d >= 0 && d < a))

      realRebase(a, digits.dropWhile(_ == 0), b)
    }.toOption
  }


}
