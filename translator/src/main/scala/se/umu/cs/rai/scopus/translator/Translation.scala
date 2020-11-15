package se.umu.cs.rai.scopus.translator

case class Translation() {

  val ScopusDefinition: String = "="

  val ScalaDefinition: String = "def"
  val ScalaValue: String = "val"

  val TranslationWithParentheses: Seq[(String, String)] = Seq(
    ("*", "class"),
    ("class", "case class"),
    ("has", "def")
  )

  val TranslationWithoutParentheses: Seq[(String, String)] = Seq(
    ("*", "class"),
    ("class", "trait"),
    ("has", "val")
  )

  val TranslationByKeyword: Seq[(String, String)] = Seq(
    (":", ":"),
    ("->", "=>"),
    ("\u2192", "=>"),
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
    ("package", "package"),
    ("import", "import"),
    ("new", "new"),
    ("@override", "override"),
    ("@tailrec", "@tailrec final"),
  )

  val ScopusReservedWords = Seq(
    "=",
    ":",
    "->",
    "\u2192",
    "*",
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
    "new"
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
