import ForthError.ForthError


class Forth extends ForthEvaluator {
  override def eval(text: String): Either[ForthError, ForthEvaluatorState] = super.eval(text)
}
