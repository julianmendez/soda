package soda.translator.parser

trait SodaConstant {

  lazy val space = " "

  lazy val new_line = "\n"

  lazy val abstract_reserved_word = "abstract"

  lazy val has_reserved_word = "has"

  lazy val class_reserved_word = "class"

  lazy val class_abbreviation = "*"

  lazy val class_begin_symbol = "{"

  lazy val class_end_symbol = "}"

  lazy val class_end_reserved_word = "end"

  lazy val class_definition_symbol = "="

  lazy val class_definition_synonym = "is"

  lazy val function_definition_symbol = "="

  lazy val function_definition_synonym = "is"

  lazy val package_reserved_word = "package"

  lazy val import_reserved_word = "import"

  lazy val import_abbreviation = "+"

  lazy val test_special_function = "test"

  lazy val theorem_reserved_word = "theorem"

  lazy val proof_reserved_word = "proof"

  lazy val comment_open = "/*"

  lazy val comment_close = "*/"

  lazy val tail_recursion_annotation = "@tailrec"

  lazy val override_annotation = "@override"

  lazy val soda_reserved_words = Seq (
    "=",
    ":",
    "->",
    "=>",
    ":=",
    "if",
    "then",
    "else",
    "let",
    "in",
    "match",
    "case",
    "end",
    "class",
    "abstract",
    "has",
    "extends",
    "with",
    "this",
    "subtype",
    "supertype",
    "false",
    "true",
    "not",
    "and",
    "or",
    "package",
    "import",
    "theorem",
    "proof",
    "is",
    "lambda",
    "<:",
    ">:",
    "+",
    "-",
    "*",
    "/",
    "|"
  )

}

case class SodaConstant_ ()
  extends SodaConstant
