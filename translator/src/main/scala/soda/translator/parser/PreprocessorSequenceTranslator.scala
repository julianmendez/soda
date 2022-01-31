package soda.translator.parser

trait PreprocessorSequenceTranslator
  extends
    soda.translator.block.BlockSequenceTranslator
{

  def   translator: soda.translator.block.BlockSequenceTranslator

  import   soda.translator.block.AnnotatedBlock

  lazy val block_annotator = BlockAnnotator_ ()

  lazy val translate: Seq [AnnotatedBlock] => Seq [AnnotatedBlock] =
     block_sequence =>
      translate_for (block_sequence )

  def translate_for (block_sequence: Seq [AnnotatedBlock]  ): Seq [AnnotatedBlock] =
    translator.translate (
      _get_second_pass (
        _get_first_pass (block_sequence )
      )
    )

  def _get_first_pass (block_sequence: Seq [AnnotatedBlock]  ): Seq [AnnotatedBlock] =
    block_sequence.map (block => block_annotator.translate (block ) )

  def _get_second_pass (block_sequence: Seq [AnnotatedBlock]  ): Seq [AnnotatedBlock] =
    block_sequence

  /* TODO */

}

case class PreprocessorSequenceTranslator_ (translator: soda.translator.block.BlockSequenceTranslator )
  extends
    PreprocessorSequenceTranslator
{

}
