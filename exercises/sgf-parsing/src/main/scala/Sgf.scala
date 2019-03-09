import scala.util.parsing.combinator.RegexParsers

object Sgf extends RegexParsers {

  override def skipWhitespace = false

  def nameParser = "[A-Z]+".r ^^ { name => name.toString }

  def justCharacterParser: Parser[String] = "[^]\\\\]".r ^^ { c => c.toString }
  def escapedCharacterParser: Parser[String] = "\\" ~ """]|[\\\snrt]""".r ^^ {
    case _ ~ escaped => escaped.toString match {
      case "\\" => "\\"
      case "]" => "]"
      case _  => ""
    }
  }

  def characterParser = escapedCharacterParser | justCharacterParser

  def valueParser = rep(characterParser) ^^ { characters => characters.mkString("") }

  def propertyParser: Parser[(String, List[String])] = nameParser ~ rep1("[" ~ valueParser ~ "]") ^^ {
    case name ~ values => (name, values.map { case _ ~ v ~ _ => v.toString}.toList)
  }

  def elementParser: Parser[SgfNode] = ";" ~ rep(propertyParser) ^^ { case _ ~ properties =>  Map(properties: _*) }

  def elementNodeParser: Parser[Node[SgfNode]] = elementParser ^^ { element => Node(element) }
  def nodeParser: Parser[Node[SgfNode]] = "(" ~ elementParser ~ rep(innerTreeParser) ~ ")" ^^ {
    case _ ~ element ~ children ~ _ => Node(element, children.toList)
  }


  def innerTreeParser: Parser[SgfTree] = nodeParser | elementNodeParser
  def treeParser: Parser[SgfTree] = nodeParser

  type Tree[A] = Node[A] // to separate the type from the constructor, cf. Haskell's Data.Tree
  type Forest[A] = List[Tree[A]]
  case class Node[A](rootLabel: A, subForest: Forest[A] = List())

  // A tree of nodes.
  type SgfTree = Tree[SgfNode]

  // A node is a property list, each key can only occur once.
  // Keys may have multiple values associated with them.
  type SgfNode = Map[String, List[String]]

  def parseSgf(text: String): Option[SgfTree] = {
    parse(treeParser, text.replaceAll("\\s", " ")) match {
      case Success(matched, _) => Some(matched)
      case sth =>
        println(sth)
        None
    }
  }
}
