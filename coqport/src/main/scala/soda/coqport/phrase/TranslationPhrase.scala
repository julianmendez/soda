package soda.coqport.phrase

trait TranslationPhrase {

  def can_apply (line: String ): Boolean

  def translate (line: String ): String

}
