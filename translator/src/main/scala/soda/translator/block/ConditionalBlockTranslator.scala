package soda.translator.block

/*
 * This package contains abstract and concrete classes to define a block and a block translator.
 */





trait ConditionalBlockTranslator
  extends
    BlockTranslator
{

  def   accepted_annotations : Seq [BlockAnnotationId]
  def   translator : BlockTranslator

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (block : AnnotatedBlock) : AnnotatedBlock =
    if ( accepted_annotations.contains (block.block_annotation)
    ) translator.translate (block)
    else block

}

case class ConditionalBlockTranslator_ (accepted_annotations : Seq [BlockAnnotationId], translator : BlockTranslator) extends ConditionalBlockTranslator
