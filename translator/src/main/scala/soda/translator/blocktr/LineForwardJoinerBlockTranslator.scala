package soda.translator.blocktr

trait LineForwardJoinerBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.block.Block_

  lazy val source = "soda"

  lazy val target = "soda"

  def translate (block: Block ): Block =
    Block_ (LineJoinerToScala_ (block.lines ) .joined_lines_with_forward_join    )

}

case class LineForwardJoinerBlockTranslator_ ()  extends LineForwardJoinerBlockTranslator
