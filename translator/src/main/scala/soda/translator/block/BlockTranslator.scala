package soda.translator.block

trait BlockTranslator
{

  def   translate: AnnotatedBlock => AnnotatedBlock

}

case class DefaultBlockTranslator_ ()
  extends
    BlockTranslator
{

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      block

}
