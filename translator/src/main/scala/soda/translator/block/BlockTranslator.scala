package soda.translator.block

trait BlockTranslator
{

  def   translate : AnnotatedBlock => AnnotatedBlock

}

case class BlockTranslator_ (translate : AnnotatedBlock => AnnotatedBlock) extends BlockTranslator

trait DefaultBlockTranslator
  extends
    BlockTranslator
{

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      block

}

case class DefaultBlockTranslator_ () extends DefaultBlockTranslator
