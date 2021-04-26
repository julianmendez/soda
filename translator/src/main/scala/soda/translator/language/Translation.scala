package soda.translator.language

/**
 * This class contains constants that are specific for the Soda translator, like reserved words for Soda and Scala.
 */
case class Translation () {

  lazy val SodaDefinition: String = "="

  lazy val SodaColon: String = ":"

  lazy val ScalaDefinition: String = "def"
  lazy val ScalaValue: String = "lazy val"
  lazy val ScalaEntryPoint: String = "object EntryPoint {\n  def main(args: Array[String]): Unit = Main().main(args)\n}\n"

  lazy val SodaReservedWords = Seq (
    "=", ":", "->", ":=", "if", "then", "else", "class", "has", "extends", "with", "this", "false", "true", "not", "and", "or", "package", "import", "new", "is", "suchthat", "*", "-", "+"
  )

  /* https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html */
  lazy val ScalaReservedWords = Seq (
    "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final", "finally", "for", "forSome", "if", "implicit", "import", "lazy", "macro", "match", "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "this", "throw", "trait", "try", "true", "type", "val", "var", "while", "with", "yield", "_", ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
  )

  lazy val SynonymAtBeginning: Seq [(String, String )] = Seq (
    ("*", "class"), ("+", "import")
  )

  lazy val TranslationAtBeginningWithParen: Seq [(String, String )] = Seq (
    ("class", "case class"), ("has", "def")
  )

  lazy val TranslationAtBeginningWithoutParen: Seq [(String, String )] = Seq (
    ("class", "trait"), ("has", "def"), ("package", "package"), ("import", "import"),
    /** Annotations */
    ("@override", "override"), ("@tailrec", "import scala.annotation.tailrec\n        @tailrec"), ("@main", ScalaEntryPoint )
  )

  lazy val TranslationBetweenSquareBrackets: Seq [(String, String )] = Seq (
    ("extends", "<:")
  )

  lazy val Synonym: Seq [(String, String )] = Seq (
    ("is", "="), ("suchthat", "->")
  )

  lazy val MainTranslation: Seq [(String, String )] = Seq (
    (":", ":"), ("->", "=>"), (":=", "="), ("if", "if ("), ("then", ")"), ("else", "else"), ("extends", "extends"), ("with", "with"), ("this", "this"), ("false", "false"), ("true", "true"), ("not", "!"), ("and", "&&"), ("or", "||"), ("new", "new")
  )

  lazy val PrefixScalaNonSoda = "__soda__"

  lazy val ScalaNonSodaKeys: Seq [String] =
    ScalaReservedWords
      .filter (x => ! SodaReservedWords.contains (x )  )

  lazy val SodaBracketsAndComma = Seq ('(', ')', '[', ']', '{', '}', ',' )

  lazy val Beautifier: Seq [(String, String )] = Seq (
    ("\\.\\s+", "."), ("=\\s+", "= "), ("\\s+=", " ="), ("\\(\\s+", "("), ("\\[\\s+", "["), ("\\s+\\]", "]"), ("\\s+,", ","), (",\\s+", ", "), ("\\s+:", ":"), (":\\s+", ": ")
  )

  def is_scala_word (word: String ): Boolean = ScalaReservedWords.contains (word )

  def is_soda_word (word: String ): Boolean = SodaReservedWords.contains (word )

}
