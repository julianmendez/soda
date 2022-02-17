package soda.translator.block

trait SingleLineProcessor
{

  def   line : String

}

case class SingleLineProcessor_ (line : String) extends SingleLineProcessor
