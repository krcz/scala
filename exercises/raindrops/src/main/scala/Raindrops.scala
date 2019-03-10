object Raindrops {

  val sounds = List(3 -> "Pling", 5 -> "Plang", 7 -> "Plong")

  def convert(n: Int): String = {
    val sound = sounds.flatMap { case (k, v) => if (n % k == 0) Some(v) else None }.mkString("")
    if (sound.nonEmpty) sound else n.toString
  }
}

