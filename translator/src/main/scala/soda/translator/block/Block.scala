package soda.translator.block

trait AnnotatedLine {

  def line: String

  def isComment: Boolean

}

case class AnnotatedLine_ (line: String, isComment: Boolean )  extends AnnotatedLine

/**
 * This preprocessor annotates lines to determine whether they are comments.
 */
trait Block  extends PlainBlock {

  def annotated_lines: Seq [AnnotatedLine]

}

case class Block_ (lines: Seq [String], annotated_lines: Seq [AnnotatedLine]  )  extends Block
