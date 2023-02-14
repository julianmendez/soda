package soda.translator.block

/*
 * This package contains abstract and concrete classes to define a block and a block translator.
 */





trait LineTranslator
{

  def   line : String

}

case class LineTranslator_ (line : String) extends LineTranslator
