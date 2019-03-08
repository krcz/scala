class Accumulate {
  def accumulate[A, B](f: (A) => B, list : List[A]): List[B] = {
    def _accumulate(rest: List[A], acc: List[B]): List[B] = {
      rest match {
        case Nil => acc.reverse
        case head :: tail =>
          _accumulate(tail, f(head) :: acc)
      }
    }
    _accumulate(list, Nil)
  }
}
