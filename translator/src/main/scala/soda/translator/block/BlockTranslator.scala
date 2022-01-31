package soda.translator.block

trait BlockTranslator
{

  def   translate: AnnotatedBlock => AnnotatedBlock

}

trait DefaultBlockTranslator
  extends
    BlockTranslator
{

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      block

}

case class DefaultBlockTranslator_ ()
  extends
    DefaultBlockTranslator
{

}
