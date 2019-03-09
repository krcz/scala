object Change {

  private def realFindFewestCoins(change: Int, coins: List[Int]): Option[List[Int]] = {
    val solutions = Array.fill[Option[(Int, Int)]](change + 1)(None)
    solutions(0) = Some((0, 0))

    for ((solution, i) <- solutions.view.zipWithIndex; (cost, _) <- solution; coin <- coins) {
      if (i + coin <= change && !solutions(i + coin).exists(s => s._1 <= cost + 1)) {
        solutions(i + coin) = Some((cost + 1, coin))
      }
    }

    def composeSolution(rest: Int, acc: List[Int]): List[Int] = {
      if (rest == 0) acc else {
        val (_, step) = solutions(rest).getOrElse(throw new Exception("Impossible state"))
        composeSolution(rest - step, step :: acc)
      }
    }

    solutions(change).map(_ => composeSolution(change, Nil).sorted)
  }

  def findFewestCoins(change: Int, coins: List[Int]): Option[List[Int]] = {
    if (change < 0) None else realFindFewestCoins(change, coins)
  }
}
