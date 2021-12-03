package soda.translator.block

trait BlockProcessor {

  import soda.lib.SomeSD_

  def translator: BlockTranslator

  lazy val new_line = "\n"

  lazy val double_new_line = new_line + new_line

  def translate (program: String ): String =
    SomeSD_ (program )
      .map (split_blocks )
      .map (translate_blocks )
      .map (join_translated_blocks )
      .value

  def split_blocks (program: String ): Seq [Block] =
    program
      .split (double_new_line )
      .toIndexedSeq
      .map (paragraph => make_block (paragraph ) )

  def make_block (paragraph: String ): Block =
    Block_ (paragraph.split (new_line )    )

  def translate_blocks (blocks: Seq [Block]  ): Seq [Block] =
    blocks.map (block => translator.translate (block ) )

  def join_translated_blocks (blocks: Seq [Block]  ): String =
    blocks
      .map (x => x.contents )
      .mkString (double_new_line ) + new_line

}

case class BlockProcessor_ (translator: BlockTranslator )  extends BlockProcessor
