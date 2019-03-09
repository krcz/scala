import scala.util.parsing.combinator._

object Alphametics extends RegexParsers {
  def equalityParser = "==" ^^ (_ => ())
  def plusParser = "+" ^^ (_ => ())
  def wordParser = "[A-Z]+".r ^^ (w => w.toString)

  def sideParser: Parser[List[String]] = wordParser ~ rep(plusParser ~ wordParser) ^^ {
    case word ~ otherWords =>
      word.toString :: otherWords.map {case _ ~ w => w.toString }.toList
  }

  def equationParser = sideParser ~ equalityParser ~ sideParser ^^ {
    case left ~ _ ~ right => (left, right)
  }

  def getCoeffitients(words: List[String]): Map[Char, Long] = {
    words.foldLeft(Map.empty[Char, Long]) { (coeffs, word) =>
      word.foldRight((1L, coeffs)) { (c, acc) =>
        val (tenPow, wcoeffs) = acc
        (10 * tenPow, wcoeffs + (c -> (wcoeffs.getOrElse(c, 0L) + tenPow)))
      } match {
        case (_, wcoeffs) => wcoeffs
      }
    }
  }

  // finds such mapping m: Map[Char, Int] with values between 0 and 9 that
  // left.map {case (c, v) => m(c) * v}.sum == right.map {case (c, v) => m(c) * v}.sum + target
  // assumptions:
  // left and right are sorted from highest to lowest on second dimension
  // target >= 0
  def solveTransformed(left: List[(Char, Long)], right: List[(Char, Long)], leading: Set[Char], available: Set[Int], target: Long): Option[Map[Char, Int]] = {
    left match {
      case _ if left.map(_._2 * 9).sum < target => None

      case Nil if target == 0 =>
        if (right.isEmpty) Some(Map.empty[Char, Int]) else solveTransformed(right, Nil, leading, available, 0)

      case (c, coefficient) :: leftTail =>
        val availableCheckZero = if (leading.contains(c)) (available - 0) else available

        val solutions = for (value <- availableCheckZero) yield {
          val solution = if (value * coefficient <= target) {
            solveTransformed(leftTail, right, leading, available - value, target - value * coefficient)
          } else {
            solveTransformed(right, leftTail, leading, available - value, value * coefficient - target)
          }
          solution.map(_ + (c -> value))
        }

        solutions.flatten.headOption
    }
  }

  def solve(equation: String): Option[Map[Char, Int]] = {
    parse(equationParser, equation) match {
      case Success((left, right), _) =>
        val leading = (left ++ right).map(_.head).toSet

        val leftCoeffitients = getCoeffitients(left)
        val rightCoeffitients = getCoeffitients(right)
        val coefficients = (leftCoeffitients.keys ++ rightCoeffitients.keys).map { c =>
          (c, leftCoeffitients.getOrElse(c, 0L) - rightCoeffitients.getOrElse(c, 0L))
        }.toList

        val (posCoeffitients, negCoeffitients) = coefficients.partition(_._2 > 0L)

        val tLeftCoeffitients = posCoeffitients.sortBy(- _._2)
        val tRightCoeffitients = negCoeffitients.map(kv => (kv._1, -kv._2)).sortBy(- _._2)

        solveTransformed(tLeftCoeffitients, tRightCoeffitients, leading, (0 to 9).toSet, 0L)
      case Failure(msg, _) => println(s"FAILURE: $msg"); None
      case Error(msg, _) => println(s"ERROR: $msg"); None
    }
  }
 }
