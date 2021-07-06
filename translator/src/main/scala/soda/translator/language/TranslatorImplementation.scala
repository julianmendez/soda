package soda.translator.language

trait Translator extends soda.translator.replacement.Translator

case class SynonymAtBeginning () extends Translator {

  lazy val _table = Translation () .SynonymAtBeginning

  lazy val keys = _keys (_table )

  def translate (word: String ): String = _get (_table, word )
}

case class TranslationAtBeginningWithParen () extends Translator {

  lazy val _table = Translation () .TranslationAtBeginningWithParen

  lazy val keys = _keys (_table )

  def translate (word: String ): String = _get (_table, word )
}

case class TranslationAtBeginningWithoutParen () extends Translator {

  lazy val _table = Translation () .TranslationAtBeginningWithoutParen

  lazy val keys = _keys (_table )

  def translate (word: String ): String = _get (_table, word )
}

case class Synonym () extends Translator {

  lazy val _table = Translation () .Synonym

  lazy val keys = _keys (_table )

  def translate (word: String ): String = _get (_table, word )
}

case class MainTranslation () extends Translator {

  lazy val _table = Translation () .MainTranslation

  lazy val keys = _keys (_table )

  def translate (word: String ): String = _get (_table, word )
}

case class ScalaNonSoda () extends Translator {

  lazy val _table = Translation () .ScalaNonSoda

  lazy val keys = _keys (_table )

  def translate (word: String ): String = _get (_table, word )
}

case class Beautifier () extends Translator {

  lazy val _table = Translation () .Beautifier

  lazy val keys = _keys (_table )

  def translate (word: String ): String = _get (_table, word )
}
