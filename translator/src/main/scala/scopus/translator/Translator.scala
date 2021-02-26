package scopus.translator


trait Translator {
  def translate (word: String): String
  val keys: Seq[String]

  def _get(table: Seq[(String, String)], word: String) =
    table.toMap.get(word).getOrElse(word)

  def _keys(table: Seq[(String, String)]) =
    table.map(pair => pair._1)
}

case class SynonymAtBeginning () extends Translator {
  val _table = Translation().SynonymAtBeginning

  def translate (word: String) = _get(_table, word)

  val keys = _keys(_table)
}

case class TranslationAtBeginningWithParen() extends Translator {
  val _table = Translation().TranslationAtBeginningWithParen

  def translate (word: String) = _get(_table, word)

  val keys = _keys(_table)
}

case class TranslationAtBeginningWithoutParen() extends Translator {
  val _table = Translation().TranslationAtBeginningWithoutParen

  def translate (word: String) = _get(_table, word)

  val keys = _keys(_table)
}


case class Synonym() extends Translator {
  val _table = Translation().Synonym

  def translate (word: String) = _get(_table, word)

  val keys = _keys(_table)
}


case class MainTranslation() extends Translator {
  val _table = Translation().MainTranslation

  def translate (word: String) = _get(_table, word)

  val keys = _keys(_table)
}


case class ScalaNonScopus() extends Translator {
  def translate (word: String) = {
    if ( ! Translation().ScopusReservedWords.contains(word) &&
      Translation().ScalaReservedWords.contains(word)
    ) Translation().PrefixScalaNonScopus + word
    else word
  }

  val keys = Translation().ScalaNonScopusKeys

}
