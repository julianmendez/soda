package soda.translator.block

/**
 * This models an abstract translator.
 */

trait Translator
{

  def   translate (word: String ): String
  def   keys: Seq [String]

}
