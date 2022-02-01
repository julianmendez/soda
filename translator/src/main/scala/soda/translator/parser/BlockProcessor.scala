package soda.translator.parser

/**
 * An instance of this class splits a String in blocks, applies a translator to them, and joins them again in a String.
 */

trait BlockProcessor
{

  def   translator: soda.translator.block.BlockSequenceTranslator

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.annotation.AnnotationFactory_

  lazy val new_line = "\n"

  lazy val double_new_line = new_line + new_line

  lazy val translator_with_preprocessor = PreprocessorSequenceTranslator_ (translator )

  def translate (program: String ): String =
    join_translated_blocks (
      translator_with_preprocessor.translate (
        split_blocks (program )
      )
    )

  def split_blocks (program: String ): Seq [AnnotatedBlock] =
    program
      .split (double_new_line )
      .toIndexedSeq
      .map (paragraph => make_block (paragraph ) )

  def make_block (paragraph: String ): AnnotatedBlock =
    AnnotationFactory_ () .annotate (
      BlockBuilder_ () .build (
        remove_empty_lines (paragraph.split (new_line ) .toIndexedSeq )
      )
    )

  def translate_blocks (blocks: Seq [AnnotatedBlock] ): Seq [AnnotatedBlock] =
    translator.translate (blocks )

  def join_translated_blocks (blocks: Seq [AnnotatedBlock] ): String =
    blocks
      .map (x => x.contents )
      .mkString (double_new_line ) + new_line

  def remove_empty_lines (lines: Seq [String] ): Seq [String] =
    lines
      .filter (line => line.trim.nonEmpty )

}

case class BlockProcessor_ (translator: soda.translator.block.BlockSequenceTranslator )
  extends
    BlockProcessor
{

}
