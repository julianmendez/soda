package soda.translator.block

trait ConditionalBlockTranslator
  extends BlockTranslator {

  def   accepted_annotations: Seq [BlockAnnotationId]
  def   translator: BlockTranslator

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    if (accepted_annotations.contains (block.block_annotation )
    ) translator.translate (block )
    else block

}

case class ConditionalBlockTranslator_ (accepted_annotations: Seq [BlockAnnotationId], translator: BlockTranslator )
  extends ConditionalBlockTranslator
