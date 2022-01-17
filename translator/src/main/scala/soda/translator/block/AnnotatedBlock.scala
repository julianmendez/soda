package soda.translator.block

trait AnnotatedBlock
  extends Block {

  def block_annotation: BlockAnnotationId

}

case class AnnotatedBlock_ (lines: Seq [String], annotated_lines: Seq [AnnotatedLine], block_annotation: BlockAnnotationId )
  extends AnnotatedBlock
