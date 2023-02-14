package soda.translator.block

/*
 * This package contains abstract and concrete classes to define a block and a block translator.
 */





trait SingleLineProcessor
{

  def   line : String

}

case class SingleLineProcessor_ (line : String) extends SingleLineProcessor
