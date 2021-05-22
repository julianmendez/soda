package soda.translator.language


/**
 * This class contains the specific implementations of Translator.
 */
case class Tr () {
  import soda.translator.replacement.Translator

  case class SynonymAtBeginning () extends Translator {
    lazy val _table = Translation () .SynonymAtBeginning

    lazy val keys = _keys (_table )

    def translate (word: String ) = _get (_table, word )
  }

  case class TranslationAtBeginningWithParen () extends Translator {
    lazy val _table = Translation () .TranslationAtBeginningWithParen

    lazy val keys = _keys (_table )

    def translate (word: String ) = _get (_table, word )
  }

  case class TranslationAtBeginningWithoutParen () extends Translator {
    lazy val _table = Translation () .TranslationAtBeginningWithoutParen

    lazy val keys = _keys (_table )

    def translate (word: String ) = _get (_table, word )
  }

  case class TranslationBetweenSquareBrackets () extends Translator {
    lazy val _table = Translation () .TranslationBetweenSquareBrackets

    lazy val keys = _keys (_table )

    def translate (word: String ) = _get (_table, word )
  }

  case class Synonym () extends Translator {
    lazy val _table = Translation () .Synonym

    lazy val keys = _keys (_table )

    def translate (word: String ) = _get (_table, word )
  }


  case class MainTranslation () extends Translator {
    lazy val _table = Translation () .MainTranslation

    lazy val keys = _keys (_table )

    def translate (word: String ) = _get (_table, word )

  }

  case class ScalaNonSoda () extends Translator {
    lazy val keys = Translation () .ScalaNonSodaKeys

    def translate (word: String ) =
      if (! Translation () .SodaReservedWords.contains (word ) &&
        Translation () .ScalaReservedWords.contains (word )
      ) Translation () .PrefixScalaNonSoda + word
      else word
  }

  case class Beautifier () extends Translator {
    lazy val _table = Translation () .Beautifier

    lazy val keys = _keys (_table )

    def translate (word: String ) = _get (_table, word )

  }
}
