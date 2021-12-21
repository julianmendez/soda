package soda.translator.blocktr

trait LineForwardJoinerBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.block.Block_

  lazy val source = "soda"

  lazy val target = "soda"

  lazy val space = " "

  lazy val reserved_word_joiner: Seq [String] = Seq ("extends", "with"  )

  lazy val symbol_forward_joiner: Seq [String] = Seq (",", "(", "["  )

  def translate (block: Block ): Block =
    Block_ (Joiner_ (block.lines, is_a_forward_join ) .join    )

  def is_a_forward_join (previous_line: String, current_line: String ): Boolean =
    is_a_symbol_forward_join (previous_line ) ||
    is_a_reserved_word_forward_join (previous_line )

  def is_a_symbol_forward_join (previous_line: String ): Boolean =
    symbol_forward_joiner
      .exists (previous_line.endsWith )

  def is_a_reserved_word_forward_join (previous_line: String ): Boolean =
    reserved_word_joiner
      .map (x => space + x )
      .exists (previous_line.endsWith )

}

case class LineForwardJoinerBlockTranslator_ ()  extends LineForwardJoinerBlockTranslator
