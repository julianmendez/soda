package soda.translator.block

trait AnnotatedBlock
  extends
    Block
{

  def   annotated_lines : Seq [AnnotatedLine]
  def   block_annotation : BlockAnnotationId

}

case class AnnotatedBlock_ (annotated_lines : Seq [AnnotatedLine], block_annotation : BlockAnnotationId) extends AnnotatedBlock
