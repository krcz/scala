import scala.annotation.tailrec

object BinarySearch {
  def find(col: Seq[Int], v: Int): Option[Int] = {

    @tailrec
    def bs(left: Int, right: Int): Option[Int] = {
      if (left >= right) {
        None
      } else {
        val middle = (left + right) / 2
        v compare col(middle) match {
          case 0 => Some(middle)
          case -1 => bs(left, middle)
          case 1 => bs(middle + 1, right)
        }
      }
    }
    bs(0, col.size)
  }
}
