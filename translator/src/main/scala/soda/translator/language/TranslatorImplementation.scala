package soda.translator.language


case class SynonymAtBeginning () extends soda.translator.replacement.Translator {

  lazy val _table = Translation () .SynonymAtBeginning

  lazy val keys = _keys (_table )

  def translate (word: String ) = _get (_table, word )
}

case class TranslationAtBeginningWithParen () extends soda.translator.replacement.Translator {

  lazy val _table = Translation () .TranslationAtBeginningWithParen

  lazy val keys = _keys (_table )

  def translate (word: String ) = _get (_table, word )
}

case class TranslationAtBeginningWithoutParen () extends soda.translator.replacement.Translator {

  lazy val _table = Translation () .TranslationAtBeginningWithoutParen

  lazy val keys = _keys (_table )

  def translate (word: String ) = _get (_table, word )
}

case class Synonym () extends soda.translator.replacement.Translator {

  lazy val _table = Translation () .Synonym

  lazy val keys = _keys (_table )

  def translate (word: String ) = _get (_table, word )
}

case class MainTranslation () extends soda.translator.replacement.Translator {

  lazy val _table = Translation () .MainTranslation

  lazy val keys = _keys (_table )

  def translate (word: String ) = _get (_table, word )
}

case class ScalaNonSoda () extends soda.translator.replacement.Translator {

  lazy val keys = Translation () .ScalaNonSodaKeys

  def translate (word: String ) =
    if (! Translation () .SodaReservedWords.contains (word ) &&
      Translation () .ScalaReservedWords.contains (word )
    ) Translation () .PrefixScalaNonSoda + word
    else word
}

case class Beautifier () extends soda.translator.replacement.Translator {

  lazy val _table = Translation () .Beautifier

  lazy val keys = _keys (_table )

  def translate (word: String ) = _get (_table, word )
}
