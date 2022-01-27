package soda.translator.block

trait BlockSequenceTranslator
{

  def   translate (block_sequence: Seq [AnnotatedBlock]  ): Seq [AnnotatedBlock]

}

trait DefaultBlockSequenceTranslator
  extends
    BlockSequenceTranslator
{

  def   translator: BlockTranslator

  def translate (block_sequence: Seq [AnnotatedBlock]  ): Seq [AnnotatedBlock] =
    block_sequence.map (block => translator.translate (block ) )

}

case class DefaultBlockSequenceTranslator_ (translator: BlockTranslator )
  extends
    DefaultBlockSequenceTranslator
{

}
