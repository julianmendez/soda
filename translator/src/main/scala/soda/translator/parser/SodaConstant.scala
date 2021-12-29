package soda.translator.parser

trait SodaConstant {

  lazy val space = " "

  lazy val new_line = "\n"

  lazy val soda_reserved_words = Seq ("=", ":", "->", "=>", ":=", "if", "then", "else", "let", "in", "match", "case", "end", "class", "has", "extends", "with", "this", "subtype", "supertype", "false", "true", "not", "and", "or", "package", "import", "is", "lambda", "<:", ">:", "+", "-", "*", "/", "|"  )

}
