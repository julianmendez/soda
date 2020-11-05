package se.umu.cs.rai.scopus.translator.tokenizer

case class Translation() {


  val TranslationOfDefinition: Map[(String, Boolean), String] = Map(
    (("=", false), "val"),
    (("=", true), "def"),
  )

  val TranslationWithParentheses: Map[String, String] = Map(
    ("has", "def"),
    ("class", "case class")
  )

  val TranslationWithoutParentheses: Map[String, String] = Map(
    ("has", "val"),
    ("class", "trait")
  )

  val TranslationByKeyword: Map[String, String] = Map(
    ("and", "&&"),
    ("else", "else"),
    ("extends", "extends"),
    ("false", "false"),
    ("if", "if ("),
    ("import", "import"),
    ("new", "new"),
    ("not", "!"),
    ("or", "||"),
    ("package", "package"),
    ("then", ")"),
    ("this", "this"),
    ("true", "true"),
    ("with", "with"),
    (":", ":"),
    ("->", "=>"),
    ("\u2192", "=>"),
    ("@override", "override"),
    ("@tailrec", "@tailrec final"),
  )

}
