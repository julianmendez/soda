package soda.translator.block

/*
 * This package contains abstract and concrete classes to define a block and a block translator.
 */





trait AnnotatedLine
{

  def   line : String
  def   is_comment : Boolean

}

case class AnnotatedLine_ (line : String, is_comment : Boolean) extends AnnotatedLine

trait Block
  extends
    PlainBlock
{

  def   annotated_lines : Seq [AnnotatedLine]

  lazy val lines : Seq [String] =
    annotated_lines
      .map (  x => x.line)

  lazy val readable_lines : Seq [AnnotatedLine] =
    annotated_lines
      .filter (  line => ! line.is_comment)

}

case class Block_ (annotated_lines : Seq [AnnotatedLine]) extends Block
