package soda.translator.block

trait BlockTranslator {

  def   translate (block: AnnotatedBlock ): AnnotatedBlock

}

case class DefaultBlockTranslator_ ()
  extends BlockTranslator {

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    block

}
