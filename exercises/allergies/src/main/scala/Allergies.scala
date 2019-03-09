object Allergen extends Enumeration(0) {
  val Eggs = Value
  val Peanuts = Value
  val Shellfish = Value
  val Strawberries = Value
  val Tomatoes = Value
  val Chocolate = Value
  val Pollen = Value
  val Cats = Value
}


object Allergies {
  def allergicTo(allergen: Allergen.Value, mask: Int): Boolean =  (mask & (1 << allergen.id)) != 0
  def list(mask: Int): List[Allergen.Value] = {
    val clippedMask = mask & ((1 << Allergen.maxId) - 1)
    Allergen.ValueSet.fromBitMask(Array(clippedMask.toLong)).toList
  }
}
