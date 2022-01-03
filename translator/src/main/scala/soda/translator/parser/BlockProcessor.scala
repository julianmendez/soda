package soda.translator.parser

trait BlockProcessor {

  import soda.lib.SomeSD_
  import soda.translator.block.AnnotatedBlock
  import soda.translator.block.AnnotatedBlock_
  import soda.translator.block.BlockTranslator
  import soda.translator.block.Block
  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.BlockBuilder_

  def translator: BlockTranslator

  lazy val new_line = "\n"

  lazy val double_new_line = new_line + new_line

  def translate (program: String ): String =
    SomeSD_ (program )
      .map (split_blocks )
      .map (translate_blocks )
      .map (join_translated_blocks )
      .value

  def split_blocks (program: String ): Seq [AnnotatedBlock] =
    program
      .split (double_new_line )
      .toIndexedSeq
      .map (paragraph => make_block (paragraph ) )

  def make_block (paragraph: String ): AnnotatedBlock =
    BlockBuilder_ () .build (paragraph.split (new_line ) .toIndexedSeq, BlockAnnotationEnum_ () .undefined    )

  def translate_blocks (blocks: Seq [AnnotatedBlock]  ): Seq [AnnotatedBlock] =
    blocks.map (block => translator.translate (block ) )

  def join_translated_blocks (blocks: Seq [AnnotatedBlock]  ): String =
    blocks
      .map (x => x.contents )
      .mkString (double_new_line ) + new_line

}

case class BlockProcessor_ (translator: soda.translator.block.BlockTranslator )  extends BlockProcessor
