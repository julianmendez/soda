package soda.translator.blocktr

trait LineBackwardJoinerBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.parser.BlockBuilder_

  lazy val source = "soda"

  lazy val target = "soda"

  lazy val space = " "

  lazy val reserved_word_joiner: Seq [String] = Seq ("extends", "with"  )

  lazy val symbol_backward_joiner: Seq [String] = Seq (")", "]"  )



  def translate (block: Block ): Block =
    BlockBuilder_ () .build (Joiner_ (block.lines, is_a_backward_join ) .join    )

  def is_a_backward_join (previous_line: String, current_line: String ): Boolean =
    is_a_symbol_backward_join (current_line ) ||
    is_a_reserved_word_backward_join (current_line )

  def is_a_symbol_backward_join (current_line: String ): Boolean =
    symbol_backward_joiner
      .exists (current_line.startsWith )

  def is_a_reserved_word_backward_join (current_line: String ): Boolean =
    reserved_word_joiner
      .map (x => x + space )
      .exists (current_line.startsWith )

}

case class LineBackwardJoinerBlockTranslator_ ()  extends LineBackwardJoinerBlockTranslator
