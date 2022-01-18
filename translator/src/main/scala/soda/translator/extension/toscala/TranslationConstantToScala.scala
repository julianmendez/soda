package soda.translator.extension.toscala

/**
 * This class contains constants that are specific for the Soda translator, like reserved words for Soda and Scala.
 */
trait TranslationConstantToScala
  extends soda.translator.parser.SodaConstant {

  lazy val soda_definition: String = "="

  lazy val soda_colon: String = ":"

  lazy val soda_opening_parenthesis: String = "("

  lazy val soda_opening_brace: String = "{"

  lazy val scala_3_class_definition: String = ":"

  lazy val soda_let_reserved_word: String = "let"

  lazy val soda_let_pattern: String = soda_let_reserved_word + " "

  lazy val soda_in_reserved_word: String = "in"

  lazy val soda_in_pattern: String = soda_in_reserved_word + " "

  lazy val scala_in_translation: String = " }"

  lazy val soda_in_let_pattern: String = soda_in_reserved_word + " " + soda_let_reserved_word + " "

  lazy val scala_in_let_translation: String = " "

  lazy val soda_match_reserved_word: String = "match"

  lazy val soda_match_pattern: String = soda_match_reserved_word + " "

  lazy val scala_match_translation: String = " match "

  lazy val soda_match_end_reserved_word: String = "end"

  lazy val scala_match_end_translation: String = "}"

  lazy val scala_opening_brace: String = "{"

  lazy val scala_closing_brace: String = "}"

  lazy val soda_class_reserved_word: String = "class"

  lazy val scala_definition: String = "def"

  lazy val scala_value: String = "lazy val"

  lazy val scala_entry_point: String = "object EntryPoint {\n  def main (args: Array [String]): Unit = Main ().main (args)\n}\n"

  /**
   * Scala 3 keywords:
   *   https://dotty.epfl.ch/docs/internals/syntax.html
   * Scala 2 keywords:
   *   https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html
   */
  lazy val scala_reserved_words =
    scala_3_regular_keywords ++ scala_3_soft_keywords ++ scala_2_extra_keywords

  lazy val scala_3_regular_keywords = Seq (
    "abstract",
    "case",
    "catch",
    "class",
    "def",
    "do",
    "else",
    "enum",
    "export",
    "extends",
    "false",
    "final",
    "finally",
    "for",
    "given",
    "if",
    "implicit",
    "import",
    "lazy",
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
    "then",
    "throw",
    "trait",
    "true",
    "try",
    "type",
    "val",
    "var",
    "while",
    "with",
    "yield",
    ":",
    "=",
    "<-",
    "=>",
    "<:",
    ">:",
    "#",
    "@",
    "=>>",
    "?=>"
  )

  lazy val scala_3_soft_keywords = Seq (
    "as",
    "derives",
    "end",
    "extension",
    "infix",
    "inline",
    "opaque",
    "open",
    "transparent",
    "using",
    "|",
    "*",
    "+",
    "-"
  )

  lazy val scala_2_extra_keywords = Seq (
    "forSome",
    "macro",
    "this",
    "_",
    "<%",
    "\u21D2",
    "\u2190"
  )

  lazy val synonym_at_beginning: Seq [(String, String )] = Seq (
    ("*", "class"),
    ("+", "import"),
    ("|", "case")
  )

  lazy val class_declaration_translation_at_beginning_with_paren = "case class"

  lazy val class_declaration_translation_at_beginning_without_paren_for_type_alias = "type"

  lazy val class_declaration_translation_at_beginning_without_paren = "trait"

  lazy val synonym: Seq [(String, String )] = Seq (
    ("is", "="),
    ("lambda", ""),
    ("<:", "subtype"),
    (">:", "supertype")
  )

  lazy val main_translation: Seq [(String, String )] = Seq (
    (":", ":"),
    ("->", "=>"),
    ("=>", "=>"),
    (":=", "="),
    ("if", "if ("),
    ("then", ")"),
    ("else", "else"),
    ("let", "{"),
    ("in", " "),
    ("match", "match"),
    ("case", "case"),
    ("end", "}"),
    ("has", "def"),
    ("extends", "extends"),
    ("with", "with"),
    ("this", "this"),
    ("subtype", "<:"),
    ("supertype", ">:"),
    ("false", "false"),
    ("true", "true"),
    ("not", "!"),
    ("and", "&&"),
    ("or", "||"),
    ("theorem", "theorem"),
    ("proof", "proof"),
    ("@new", "new"),
    ("@override", "override"),
    ("@tailrec", "import scala.annotation.tailrec\n        @tailrec  final"),
    ("@main", scala_entry_point )
  )

  lazy val prefix_scala_non_soda = "__soda__"

  lazy val scala_non_soda: Seq [(String, String )] =
    scala_reserved_words
      .filter (x => ! soda_reserved_words.contains (x )  )
      .map (x =>  (x, prefix_scala_non_soda + x ) )

  lazy val soda_brackets_and_comma = Seq ('(', ')', '[', ']', '{', '}', ',' )

  lazy val beautifier: Seq [(String, String )] = Seq (
    ("\\.\\s+", "."),
    ("=\\s+", "= "),
    ("\\s+=", " ="),
    ("\\(\\s+", "("),
    ("\\[\\s+", "["),
    ("\\s+\\]", "]"),
    ("\\s+,", ","),
    (",\\s+", ", "),
    ("\\s+:", ":"),
    (":\\s+", ": ")
  )

  def is_scala_word (word: String ): Boolean =
    scala_reserved_words.contains (word )

  def is_soda_word (word: String ): Boolean =
    soda_reserved_words.contains (word )

}

case class TranslationConstantToScala_ ()
  extends TranslationConstantToScala
