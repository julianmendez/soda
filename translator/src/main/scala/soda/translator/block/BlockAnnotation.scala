package soda.translator.block

trait BlockAnnotation
{

  def   block: soda.translator.block.Block
  def   applies: Boolean
  def   identifier: soda.translator.block.BlockAnnotationId

}
