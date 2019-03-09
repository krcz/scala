object Anagram {
  def findAnagrams(word: String, candidates: List[String]): List[String] = {
    def letterCounts(w: String) = w.toLowerCase.groupBy(identity).mapValues(_.size)
    val wordLetterCounts = letterCounts(word)
    def isWordAnagram(w: String) = w.toLowerCase != word.toLowerCase && letterCounts(w) == wordLetterCounts

    candidates.filter(isWordAnagram)
  }
}
