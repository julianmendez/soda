package soda.translator.language

import soda.translator.replacement.Translator

case class SynonymAtBeginning (  ) extends Translator {
  lazy val _table = Translation (  ) .SynonymAtBeginning

  def translate ( word: String ) = _get ( _table , word )

  lazy val keys = _keys ( _table )
}

case class TranslationAtBeginningWithParen (  ) extends Translator {
  lazy val _table = Translation (  ) .TranslationAtBeginningWithParen

  def translate ( word: String ) = _get ( _table , word )

  lazy val keys = _keys ( _table )
}

case class TranslationAtBeginningWithoutParen (  ) extends Translator {
  lazy val _table = Translation (  ) .TranslationAtBeginningWithoutParen

  def translate ( word: String ) = _get ( _table , word )

  lazy val keys = _keys ( _table )
}


case class TranslationBetweenSquareBrackets (  ) extends Translator {
 lazy val _table = Translation (  ) .TranslationBetweenSquareBrackets

  def translate ( word: String ) = _get ( _table , word )

  lazy val keys = _keys ( _table )
}


case class Synonym (  ) extends Translator {
  lazy val _table = Translation (  ) .Synonym

  def translate ( word: String ) = _get ( _table , word )

  lazy val keys = _keys ( _table )
}


case class MainTranslation (  ) extends Translator {
  lazy val _table = Translation (  ) .MainTranslation

  def translate ( word: String ) = _get ( _table , word )

  lazy val keys = _keys ( _table )
}


case class ScalaNonSoda (  ) extends Translator {
  def translate ( word: String ) = {
    if ( ! Translation (  ) .SodaReservedWords.contains ( word ) &&
      Translation (  ) .ScalaReservedWords.contains ( word )
    ) Translation (  ) .PrefixScalaNonSoda + word
    else word
  }

  lazy val keys = Translation (  ) .ScalaNonSodaKeys

}

case class Beautifier (  ) extends Translator {
  lazy val _table = Translation (  ) .Beautifier

  def translate ( word: String ) = _get ( _table , word )

  lazy val keys = _keys ( _table )
}
