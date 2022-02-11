package soda.translator.block

trait LineTranslator
{

  def   line: String

}

case class LineTranslator_ (line: String) extends LineTranslator
