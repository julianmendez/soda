package se.umu.cs.rai.scopus.translator.tokenizer


case class Classifier() {

  val ScopusReservedWords = Set(
    "and",
    "class",
    "else",
    "extends",
    "false",
    "if",
    "import",
    "new",
    "not",
    "or",
    "package",
    "then",
    "this",
    "true",
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

