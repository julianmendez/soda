package soda.coqport.language


/**
 * This class contains constants that are specific for the Soda translator, like reserved words for Soda and Scala.
 */
trait TranslationConstant {

  lazy val soda_definition: String = "="

  lazy val soda_colon: String = ":"

  lazy val soda_opening_parenthesis: String = "("

  lazy val soda_opening_brace: String = "{"

  lazy val scala_3_class_definition: String = ":"

  lazy val soda_in_reserved_word: String = "in"

  lazy val soda_in_pattern: String = soda_in_reserved_word + " "

  lazy val scala_in_translation: String = " }"

  lazy val soda_class_reserved_word: String = "class"

  lazy val coq_definition: String = "Definition"

  lazy val scala_value: String = "lazy val"

  lazy val scala_entry_point: String = "object EntryPoint {\n  def main(args: Array[String]): Unit = Main().main(args)\n}\n"

  lazy val soda_reserved_words = Seq ("=", ":", "->", ":=", "if", "then", "else", "let", "in", "class", "has", "extends", "with", "this", "subtype", "supertype", "false", "true", "not", "and", "or", "package", "import", "is", "<:", ">:", "+", "-", "*", "/"  )

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

  lazy val synonym_at_beginning: Seq [(String, String )] = Seq (("*", "class"), ("+", "import")  )

  lazy val translation_at_beginning_with_paren: Seq [(String, String )] = Seq (("class", "case class"), ("has", "def")  )

  lazy val translation_at_beginning_without_paren_for_type_alias: Seq [(String, String )] = Seq (("class", "type")  )

  lazy val translation_at_beginning_without_paren: Seq [(String, String )] = Seq (("class", "trait"), ("has", "def"), ("package", "package"), ("import", "import"),
    /** Annotations */
    ("@override", "override"), ("@tailrec", "import scala.annotation.tailrec\n        @tailrec  final"), ("@main", scala_entry_point )  )

  lazy val synonym: Seq [(String, String )] = Seq (("is", "="), ("<:", "subtype"), (">:", "supertype")  )

  lazy val main_translation: Seq [(String, String )] = Seq ((":", ":"), ("->", "=>"), (":=", "="), ("if", "if"), ("then", "then"), ("else", "else"), ("let", "let"), ("in", "in"), ("extends", "extends"), ("with", "with"), ("this", "this"), ("subtype", "<:"), ("supertype", ">:"), ("false", "false"), ("true", "true"), ("not", "!"), ("and", "/\\"), ("or", "\\/"),
    /** Annotations */
    ("@new", "new")  )

  lazy val prefix_scala_non_soda = "__soda__"

  lazy val scala_non_soda: Seq [(String, String )] =
    scala_reserved_words
      .filter (x => ! soda_reserved_words.contains (x )  )
      .map (x => (x, prefix_scala_non_soda + x ) )

  lazy val soda_brackets_and_comma = Seq ('(', ')', '[', ']', '{', '}', ',' )

  lazy val beautifier: Seq [(String, String )] = Seq (("\\.\\s+", "."), ("=\\s+", "= "), ("\\s+=", " ="), ("\\(\\s+", "("), ("\\[\\s+", "["), ("\\s+\\]", "]"), ("\\s+,", ","), (",\\s+", ", "), ("\\s+:", ":"), (":\\s+", ": ")  )

  lazy val reserved_word_joiner: Seq [String] = Seq ("extends", "with"  )

  lazy val symbol_forward_joiner: Seq [String] = Seq (",", "(", "["  )

  lazy val symbol_backward_joiner: Seq [String] = Seq (")", "]"  )

  def is_scala_word (word: String ): Boolean =
    scala_reserved_words.contains (word )

  def is_soda_word (word: String ): Boolean =
    soda_reserved_words.contains (word )
}

case class TranslationConstant_ () extends TranslationConstant
