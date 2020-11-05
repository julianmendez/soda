package se.umu.cs.rai.scopus.translator.tokenizer

case class Translation() {

  val ScopusDefinition: String = "="

  val ScalaDefinition: String = "def "
  val ScalaValue: String = "val "

  val TranslationOfDefinition: Map[(String, Boolean), String] = Map(
    (("=", false), "val"),
    (("=", true), "def"),
  )

  val TranslationWithParentheses: Map[String, String] = Map(
    ("class", "case class"),
    ("has", "def")
  )

  val TranslationWithoutParentheses: Map[String, String] = Map(
    ("class", "trait"),
    ("has", "val")
  )

  val TranslationByKeyword: Map[String, String] = Map(
    ("and", "&&"),
    ("else", "else"),
    ("extends", "extends"),
    ("false", "false"),
    ("if", "if ("),
    ("import", "import"),
    ("new", "new"),
    ("not", "!"),
    ("or", "||"),
    ("package", "package"),
    ("then", ")"),
    ("this", "this"),
    ("true", "true"),
    ("with", "with"),
    (":", ":"),
    ("->", "=>"),
    ("\u2192", "=>"),
    ("@override", "override"),
    ("@tailrec", "@tailrec final"),
  )

  val ScopusReservedWords = Set(
    "and",
    "class",
    "else",
    "extends",
    "false",
    "has",
    "if",
    "import",
    "new",
    "not",
    "or",
    "package",
    "then",
    "this",
    "true",
    "with",
    ":",
    "=",
    "->",
    "\u2192"
  )

  // https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html
  val ScalaReservedWords = Set(
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
