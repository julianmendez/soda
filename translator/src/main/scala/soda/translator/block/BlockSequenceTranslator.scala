package soda.translator.block

/*
 * This package contains abstract and concrete classes to define a block and a block translator.
 */





trait BlockSequenceTranslator
{

  def   translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock]

}

case class BlockSequenceTranslator_ (translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock]) extends BlockSequenceTranslator

trait DefaultBlockSequenceTranslator
  extends
    BlockSequenceTranslator
{

  def   translator : BlockTranslator

  lazy val translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock] =
     block_sequence =>
      block_sequence.map (  block => translator.translate (block) )

}

case class DefaultBlockSequenceTranslator_ (translator : BlockTranslator) extends DefaultBlockSequenceTranslator
