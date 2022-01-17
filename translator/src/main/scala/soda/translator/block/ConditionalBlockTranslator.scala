package soda.translator.block

trait ConditionalBlockTranslator
  extends BlockTranslator {

  def accepted_annotation: BlockAnnotationId

  def translator: BlockTranslator

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    if (block.block_annotation == accepted_annotation
    )  translator.translate (block )
    else block

}

case class ConditionalBlockTranslator_ (accepted_annotation: BlockAnnotationId, translator: BlockTranslator )
  extends ConditionalBlockTranslator
