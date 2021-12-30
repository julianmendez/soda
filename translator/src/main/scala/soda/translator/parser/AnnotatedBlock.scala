package soda.translator.parser

trait AnnotatedBlock  extends soda.translator.block.Block {

  import soda.translator.parser.annotation.BlockAnnotationId

  def annotation: BlockAnnotationId

}

case class AnnotatedBlock_ (lines: Seq [String], annotated_lines: Seq [soda.translator.block.AnnotatedLine], annotation: soda.translator.parser.annotation.BlockAnnotationId )  extends AnnotatedBlock
