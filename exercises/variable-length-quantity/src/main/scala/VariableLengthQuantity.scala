import scala.util.{Either, Left, Right}

object VariableLengthQuantity {
  def encode(value: Int, acc: List[Int] = Nil): List[Int] = {
    val newByte = (value & 0x7f) | (if (acc.isEmpty) 0 else 0x80)
    if ((value >>> 7) == 0) {
      (newByte :: acc)
    } else {
      encode(value >>> 7, newByte :: acc)
    }
  }

  def encode(values: List[Int]): List[Int] = {
    values.flatMap(encode(_))
  }

  def decode(values: List[Int]): Either[String, List[Int]] = {
    def _dec(vs: List[Int], current: Option[Int], acc: List[Int]): Either[String, List[Int]] = {
      vs match {
        case Nil => current.fold[Either[String, List[Int]]](Right(acc.reverse))(_ => Left("Incomplete sequence"))
        case head :: tail if (head & 0x80) != 0 =>
          _dec(tail, Some((current.getOrElse(0) << 7) | (head & 0x7f)), acc)
        case head :: tail =>
          _dec(tail, None, ((current.getOrElse(0) << 7) | head) :: acc)
      }
    }
    _dec(values, None, Nil)
  }
}
