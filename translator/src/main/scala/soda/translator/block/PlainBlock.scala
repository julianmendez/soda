package soda.translator.block

/*
 * This package contains abstract and concrete classes to define a block and a block translator.
 */





trait PlainBlock
{

  def   lines : Seq [String]

  lazy val new_line = "\n"

  lazy val contents : String =
    lines.mkString (new_line)

}

case class PlainBlock_ (lines : Seq [String]) extends PlainBlock
