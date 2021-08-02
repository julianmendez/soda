package soda.translator.language


/**
 * This class contains constants that are specific for the Soda translator, like reserved words for Soda and Scala.
 */
trait TranslationConstant {

  lazy val soda_definition: String = "="

  lazy val soda_colon: String = ":"

  lazy val soda_in_reserved_word: String = "in"
  lazy val soda_in_pattern: String = soda_in_reserved_word + " "
  lazy val scala_in_translation: String = " }"

  lazy val soda_class_reserved_word: String = "class"

  lazy val scala_definition: String = "def"
  lazy val scala_value: String = "lazy val"
  lazy val scala_entry_point: String = "object EntryPoint {\n  def main(args: Array[String]): Unit = Main().main(args)\n}\n"

  lazy val soda_reserved_words = Seq ("=", ":", "->", ":=", "if", "then", "else", "let", "in", "class", "has", "extends", "with", "this", "subtype", "supertype", "false", "true", "not", "and", "or", "package", "import", "is", "*", "-", "+"  )

  /**
   * Scala 3 keywords:
   *   https://dotty.epfl.ch/docs/internals/syntax.html
   * Scala 2 keywords:
   *   https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html
   */
  lazy val scala_reserved_words =
    scala_3_regular_keywords ++ scala_3_soft_keywords ++ scala_2_extra_keywords

  lazy val scala_3_regular_keywords = Seq ("abstract", "case", "catch", "class", "def", "do", "else", "enum", "export", "extends", "false", "final", "finally", "for", "given", "if", "implicit", "import", "lazy", "match", "new", "null", "object", "override", "package", "private", "protected", "return", "sealed", "super", "then", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield", ":", "=", "<-", "=>", "<:", ">:", "#", "@", "=>>", "?=>"  )

  lazy val scala_3_soft_keywords = Seq ("as", "derives", "end", "extension", "infix", "inline", "opaque", "open", "transparent", "using", "|", "*", "+", "-"  )

  lazy val scala_2_extra_keywords = Seq ("forSome", "macro", "this", "_", "<%", "\u21D2", "\u2190"  )

  lazy val SynonymAtBeginning: Seq [(String, String )] = Seq (("*", "class"), ("+", "import")  )

  lazy val TranslationAtBeginningWithParen: Seq [(String, String )] = Seq (("class", "case class"), ("has", "def")  )

  lazy val TranslationAtBeginningWithoutParen: Seq [(String, String )] = Seq (("class", "trait"), ("has", "def"), ("package", "package"), ("import", "import"),
    /** Annotations */
    ("@override", "override"), ("@tailrec", "import scala.annotation.tailrec\n        @tailrec"), ("@main", scala_entry_point )  )

  lazy val Synonym: Seq [(String, String )] = Seq (("is", "=")  )

  lazy val MainTranslation: Seq [(String, String )] = Seq ((":", ":"), ("->", "=>"), (":=", "="), ("if", "if ("), ("then", ")"), ("else", "else"), ("let", "{"), ("in", " "), ("extends", "extends"), ("with", "with"), ("this", "this"), ("subtype", "<:"), ("supertype", ">:"), ("false", "false"), ("true", "true"), ("not", "!"), ("and", "&&"), ("or", "||"),
    /** Annotations */
    ("@new", "new")  )

  lazy val PrefixScalaNonSoda = "__soda__"

  lazy val ScalaNonSoda: Seq [(String, String )] =
    scala_reserved_words
      .filter (x => ! soda_reserved_words.contains (x )  )
      .map (x => (x, PrefixScalaNonSoda + x ) )

  lazy val SodaBracketsAndComma = Seq ('(', ')', '[', ']', '{', '}', ',' )

  lazy val Beautifier: Seq [(String, String )] = Seq (("\\.\\s+", "."), ("=\\s+", "= "), ("\\s+=", " ="), ("\\(\\s+", "("), ("\\[\\s+", "["), ("\\s+\\]", "]"), ("\\s+,", ","), (",\\s+", ", "), ("\\s+:", ":"), (":\\s+", ": ")  )

  lazy val ReservedWordJoiner: Seq [String] = Seq ("extends", "with"  )

  lazy val SymbolForwardJoiner: Seq [String] = Seq (",", "(", "["  )

  lazy val SymbolBackwardJoiner: Seq [String] = Seq (")", "]"  )

  def is_scala_word (word: String ): Boolean =
    scala_reserved_words.contains (word )

  def is_soda_word (word: String ): Boolean =
    soda_reserved_words.contains (word )
}

case class Translation () extends TranslationConstant
