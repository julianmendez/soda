package soda.translator.replacement

/**
 * This models an abstract translator.
 */
trait Translator {
  def translate (word: String ): String
  def keys: Seq [String]

  def _get (table: Seq [(String, String )], word: String ) =
    table.toMap.get (word ) .getOrElse (word )

  def _keys (table: Seq [(String, String )]  ) =
    table.map (pair => pair._1 )
}
