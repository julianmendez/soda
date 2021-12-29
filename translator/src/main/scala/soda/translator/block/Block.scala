package soda.translator.block

trait AnnotatedLine {

  def line: String

  def isComment: Boolean

}

case class AnnotatedLine_ (line: String, isComment: Boolean )  extends AnnotatedLine

trait Block  extends PlainBlock {

  def annotated_lines: Seq [AnnotatedLine]

  lazy val readable_lines: Seq [AnnotatedLine] =
    annotated_lines
      .filter (line => ! line.isComment )

}

case class Block_ (lines: Seq [String], annotated_lines: Seq [AnnotatedLine]  )  extends Block
