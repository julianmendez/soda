package se.umu.cs.rai.scopus.translator.tokenizer

case class Translation() {


  val TranslationByLine: Map[(String, Boolean), String] = Map(
    (("=", false), "val"),
    (("=", true), "def"),
    (("class", false), "trait"),
    (("class", true), "case class")
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
    (":", ":"),
    ("->", "=>"),
    ("\u2192", "=>")
  )

}
