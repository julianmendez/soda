package soda.translator.parser

trait SodaConstant {

  lazy val space = " "

  lazy val new_line = "\n"

  lazy val has_reserved_word = "has"

  lazy val class_reserved_word = "class"

  lazy val class_abbreviation = "*"

  lazy val class_open_symbol = "{"

  lazy val class_close_symbol = "}"

  lazy val function_definition_symbol = "="

  lazy val package_reserved_word = "package"

  lazy val import_reserved_word = "import"

  lazy val import_abbreviation = "+"

  lazy val test_special_function = "test"

  lazy val soda_reserved_words = Seq ("=", ":", "->", "=>", ":=", "if", "then", "else", "let", "in", "match", "case", "end", "class", "has", "extends", "with", "this", "subtype", "supertype", "false", "true", "not", "and", "or", "package", "import", "is", "lambda", "<:", ">:", "+", "-", "*", "/", "|"  )

}

case class SodaConstant_ ()  extends SodaConstant
