package soda.coqport.phrase

trait DefinitionPhrase  extends TranslationPhrase {

  lazy val definition_beginning = "Definition"

  lazy val space = " "

  lazy val line_end = "."

  def translate (line: String ): String =
    definition_beginning + space + line + space + line_end
}
