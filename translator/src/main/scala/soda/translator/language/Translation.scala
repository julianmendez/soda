package soda.translator.language


/**
 * This class contains constants that are specific for the Soda translator, like reserved words for Soda and Scala.
 */
case class Translation () {

  lazy val SodaDefinition: String = "="

  lazy val SodaColon: String = ":"

  lazy val SodaInReservedWord: String = "in"
  lazy val SodaInPattern: String = SodaInReservedWord + " "
  lazy val ScalaInTranslation: String = " }"

  lazy val SodaClassReservedWord: String = "class"

  lazy val ScalaDefinition: String = "def"
  lazy val ScalaValue: String = "lazy val"
  lazy val ScalaEntryPoint: String = "object EntryPoint {\n  def main(args: Array[String]): Unit = Main().main(args)\n}\n"

  lazy val SodaReservedWords = Seq ("=", ":", "->", ":=", "if", "then", "else", "let", "in", "class", "has", "extends", "with", "this", "subtype", "supertype", "false", "true", "not", "and", "or", "package", "import", "is", "*", "-", "+"  )

/**
 * Scala 3 keywords:
 *   https://dotty.epfl.ch/docs/internals/syntax.html
 * Scala 2 keywords:
 *   https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html
 */
  lazy val ScalaReservedWords =
    Scala3RegularKeywords ++ Scala3SoftKeywords ++ Scala2ExtraKeywords

  lazy val Scala3RegularKeywords = Seq ("abstract", "case", "catch", "class", "def", "do", "else", "enum", "export", "extends", "false", "final", "finally", "for", "given", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "then", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield", ":", "=", "<-", "=>", "<:", ">:", "#", "@", "=>>", "?=>"  )

  lazy val Scala3SoftKeywords = Seq ("as", "derives", "end", "extension", "infix", "inline", "opaque", "open", "transparent", "using", "|", "*", "+", "-"  )

  lazy val Scala2ExtraKeywords = Seq ("forSome", "macro", "this", "_", "<%", "\u21D2", "\u2190"  )

  lazy val SynonymAtBeginning: Seq [(String, String )] = Seq (("*", "class"), ("+", "import")  )

  lazy val TranslationAtBeginningWithParen: Seq [(String, String )] = Seq (("class", "case class"), ("has", "def")  )

  lazy val TranslationAtBeginningWithoutParen: Seq [(String, String )] = Seq (("class", "trait"), ("has", "def"), ("package", "package"), ("import", "import"),
    /** Annotations */
    ("@override", "override"), ("@tailrec", "import scala.annotation.tailrec\n        @tailrec"), ("@main", ScalaEntryPoint )  )

  lazy val Synonym: Seq [(String, String )] = Seq (("is", "=")  )

  lazy val MainTranslation: Seq [(String, String )] = Seq ((":", ":"), ("->", "=>"), (":=", "="), ("if", "if ("), ("then", ")"), ("else", "else"), ("let", "{"), ("in", " "), ("extends", "extends"), ("with", "with"), ("this", "this"), ("subtype", "<:"), ("supertype", ">:"), ("false", "false"), ("true", "true"), ("not", "!"), ("and", "&&"), ("or", "||"),
    /** Annotations */
    ("@new", "new")  )

  lazy val PrefixScalaNonSoda = "__soda__"

  lazy val ScalaNonSodaKeys: Seq [String] =
    ScalaReservedWords
      .filter (x => ! SodaReservedWords.contains (x )  )

  lazy val SodaBracketsAndComma = Seq ('(', ')', '[', ']', '{', '}', ',' )

  lazy val Beautifier: Seq [(String, String )] = Seq (("\\.\\s+", "."), ("=\\s+", "= "), ("\\s+=", " ="), ("\\(\\s+", "("), ("\\[\\s+", "["), ("\\s+\\]", "]"), ("\\s+,", ","), (",\\s+", ", "), ("\\s+:", ":"), (":\\s+", ": ")  )

  lazy val ForwardJoiner: Seq [String] = Seq (",", "(", "["  )

  lazy val BackwardJoiner: Seq [String] = Seq (")", "]"  )

  def is_scala_word (word: String ): Boolean =
    ScalaReservedWords.contains (word )

  def is_soda_word (word: String ): Boolean =
    SodaReservedWords.contains (word )
}
