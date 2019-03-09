import ForthError.{DivisionByZero, ForthError, InvalidWord, ParseError, StackUnderflow, UnknownWord}

import scala.annotation.tailrec
import scala.util.parsing.combinator.RegexParsers

object ForthError extends Enumeration {
  type ForthError = Value
  val DivisionByZero, StackUnderflow, InvalidWord, UnknownWord, ParseError = Value
}

case class ForthEvaluatorState(stack: List[Int]) {
  override def toString: String = stack.reverse.mkString(" ")
}

/** Representation of evaluated definition understood as some transformation of the stack */
abstract class Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState]
}

abstract class BinaryOperation(act: (Int, Int) => Either[ForthError, Int]) extends Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState] = {
    state.flatMap {
      case ForthEvaluatorState(a :: b :: tail) => act(b, a).map(result => (ForthEvaluatorState(result :: tail)))
      case _ => Left(StackUnderflow)
    }
  }
}

object Add extends BinaryOperation((a, b) => Right(a + b))
object Sub extends BinaryOperation((a, b) => Right(a - b))
object Mul extends BinaryOperation((a, b) => Right(a * b))
object Div extends BinaryOperation((left, right) => if (right == 0) Left(DivisionByZero) else Right(left / right))

object Dup extends Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState] = {
    state.flatMap {
      case ForthEvaluatorState(a :: tail) => Right(ForthEvaluatorState(a :: a :: tail))
      case _ => Left(StackUnderflow)
    }
  }
}

object Drop extends Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState] = {
    state.flatMap {
      case ForthEvaluatorState(a :: tail) => Right(ForthEvaluatorState(tail))
      case _ => Left(StackUnderflow)
    }
  }
}

object Over extends Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState] = {
    state.flatMap {
      case ForthEvaluatorState(a :: b :: tail) => Right(ForthEvaluatorState(b :: a :: b :: tail))
      case _ => Left(StackUnderflow)
    }
  }
}

object Swap extends Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState] = {
    state.flatMap {
      case ForthEvaluatorState(a :: b :: tail) => Right(ForthEvaluatorState(b :: a :: tail))
      case _ => Left(StackUnderflow)
    }
  }
}

case class PushNumberDefinition(value: Int) extends Definition {
  def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState] = {
    state.map(s => ForthEvaluatorState(value :: s.stack))
  }
}

case class CustomDefinition(code: List[Definition]) extends Definition {
  override def evaluate(state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState] = {
    def evalRest(rest: List[Definition], state: Either[ForthError, ForthEvaluatorState]): Either[ForthError, ForthEvaluatorState] = {
      rest match {
        case Nil => state
        case head :: tail => evalRest(tail, head.evaluate(state))
      }
    }
    evalRest(code, state)
  }
}


class ForthEvaluator extends RegexParsers {
  def wordParser: Parser[String] = "[^;:\\s]+".r ^^ { _.toString.toLowerCase }
  def definitionParser: Parser[List[String]] = ":" ~ rep1(wordParser) ~ ";" ^^ {
    case _ ~ words ~ _ => words.toList
  }
  def programParser: Parser[(List[List[String]], List[String])] = rep(definitionParser) ~ rep(wordParser) ^^ {
    case definitions ~ words => (definitions.toList, words.toList)
  }

  val builtinsContext: Map[String, Definition] = Map(
    "+" -> Add,
    "-" -> Sub,
    "*" -> Mul,
    "/" -> Div,
    "dup" -> Dup,
    "drop" -> Drop,
    "over" -> Over,
    "swap" -> Swap
  )

  /** Works like foldLeft in List, but stops on first `f` failure understood by returning Left */
  def eitherFoldLeft[A, Acc, Err](acc: Acc, col: List[A])(f: (Acc, A) => Either[Err, Acc]): Either[Err, Acc] = {
    @tailrec
    def _fold(rest: List[A], acc: Acc): Either[Err, Acc] = {
      rest match {
        case Nil => Right(acc)
        case head :: tail => f(acc, head) match {
          case Left(err) => Left(err)
          case Right(newAcc) => _fold(tail, newAcc)
        }
      }
    }
    _fold(col, acc)
  }

  /** Works like map in List, but stops on first `f` failure understood by returning Left */
  def eitherMap[A, B, Err](col: List[A])(f: A => Either[Err, B]): Either[Err, List[B]] = {
    eitherFoldLeft(List.empty[B], col) { (acc, a) => f(a).map(_ :: acc) }.map(_.reverse)
  }

  /** Evaluates definition in current context and adds in to the context */
  def evaluateDefinition(
      context: Map[String, Definition],
      definition: List[String]): Either[ForthError, Map[String, Definition]] = {
    val name :: code = definition
    if (name.forall(_.isDigit)) {
      Left(InvalidWord)
    } else {
      val evaluatedCode = eitherMap(code) { word =>
        if (word.forall(_.isDigit)) {
          Right(PushNumberDefinition(word.toInt))
        } else {
          context.get(word).fold[Either[ForthError, Definition]](Left(UnknownWord))(Right(_))
        }
      }
      evaluatedCode.map(ec => context + (name -> CustomDefinition(ec)))
    }
  }

  /** Evaluates sequence of definitions adding them to the context */
  def evaluateDefinitions(
      definitions: List[List[String]],
      context: Map[String, Definition]): Either[ForthError, Map[String, Definition]] = {
    eitherFoldLeft(context, definitions)(evaluateDefinition)
  }

  /** Evaluates program in current context */
  def evaluateProgram(
      context: Map[String, Definition],
      program: List[String]): Either[ForthError, ForthEvaluatorState] = {
    eitherFoldLeft(ForthEvaluatorState(Nil), program) { (state, word) =>
      println(s"$word:")
      val newState = if (word.forall(_.isDigit)) {
        Right(ForthEvaluatorState(word.toInt :: state.stack))
      } else {
        context
          .get(word)
          .fold[Either[ForthError, Definition]](Left(UnknownWord))(Right(_))
          .flatMap { command =>
            command.evaluate(Right(state))
          }
      }
      newState
    }
  }

  def eval(text: String): Either[ForthError, ForthEvaluatorState] = {
    parse(programParser, text) match {
      case Failure(msg, _) => println(s"FAILURE: $msg"); Left(ParseError)
      case Error(msg, _) => println(s"ERROR: $msg"); Left(ParseError)
      case Success((definitions, program), _) =>
        for {
          context <- evaluateDefinitions(definitions, builtinsContext)
          result <- evaluateProgram(context, program)
        } yield result
    }
  }
}
