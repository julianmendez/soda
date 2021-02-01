package se.umu.cs.rai.scopus.translator

case class Translation() {

  val ScopusDefinition: String = "="

  val ScalaDefinition: String = "def"
  val ScalaValue: String = "val"
  val ScalaEntryPoint: String = "object EntryPoint {\n  def main(args: Array[String]): Unit = Main().main(args)\n}\n"

  def SynonymAtBeginning: Seq[(String, String)] = Seq(
    ("*", "class"),
    ("-", "has"),
    ("+", "import")
  )

  def TranslationAtBeginningWithParen: Seq[(String, String)] = Seq(
    ("class", "case class"),
    ("has", "def")
  )

  def TranslationAtBeginningWithoutParen: Seq[(String, String)] = Seq(
    ("class", "trait"),
    ("has", "val"),
    ("package", "package"),
    ("import", "import"),

    /** Annotations */
    ("@override", "override"),
    ("@tailrec", "@tailrec final"),
    ("@main", ScalaEntryPoint)
  )


  def Synonym: Seq[(String, String)] = Seq(
    ("is", "="),
    ("in", ":"),
    ("\u2208", ":"),
    ("to", "->"),
    ("suchthat", "->"),
    ("\u2192", "->")
  )

  def Translation: Seq[(String, String)] = Seq(
    (":", ":"),
    ("->", "=>"),
    ("if", "if ("),
    ("then", ")"),
    ("else", "else"),
    ("extends", "extends"),
    ("with", "with"),
    ("this", "this"),
    ("false", "false"),
    ("true", "true"),
    ("not", "!"),
    ("and", "&&"),
    ("or", "||"),
    ("new", "new")
  )

  val ScopusReservedWords = Seq(
    "=",
    ":",
    "->",
    "if",
    "then",
    "else",
    "class",
    "has",
    "extends",
    "with",
    "this",
    "false",
    "true",
    "not",
    "and",
    "or",
    "package",
    "import",
    "new",
    "is",
    "in",
    "\u2208",
    "to",
    "suchthat",
    "\u2192",
    "*",
    "-",
    "+"
  )

  // https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html
  val ScalaReservedWords = Seq(
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "forSome",
    "if",
    "implicit",
    "import",
    "lazy",
    "macro",
    "match",
    "new",
    "null",
    "object",
    "override",
    "package",
    "private",
    "protected",
    "return",
    "sealed",
    "super",
    "this",
    "throw",
    "trait",
    "try",
    "true",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield",
    "_",
    ":",
    "=",
    "=>",
    "<-",
    "<:",
    "<%",
    ">:",
    "#",
    "@",
    "\u21D2",
    "\u2190"
  )

  def isScalaWord(word: String): Boolean = ScalaReservedWords.contains(word)

  def isScopusWord(word: String): Boolean = ScopusReservedWords.contains(word)

}
