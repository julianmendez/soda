package soda.translator.block

trait BlockSequenceTranslator
{

  def   translate: Seq [AnnotatedBlock] => Seq [AnnotatedBlock]

}

trait DefaultBlockSequenceTranslator
  extends
    BlockSequenceTranslator
{

  def   translator: BlockTranslator

  lazy val translate: Seq [AnnotatedBlock] => Seq [AnnotatedBlock] =
     block_sequence =>
      block_sequence.map (block => translator.translate (block ) )

}

case class DefaultBlockSequenceTranslator_ (translator: BlockTranslator )
  extends
    DefaultBlockSequenceTranslator
{

}
