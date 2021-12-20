package soda.translator.blocktr

trait LineBackwardJoinerBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.block.Block_

  lazy val source = "soda"

  lazy val target = "soda"

  def translate (block: Block ): Block =
    Block_ (LineJoinerToScala_ (block.lines ) .joined_lines_with_backward_join    )

}

case class LineBackwardJoinerBlockTranslator_ ()  extends LineBackwardJoinerBlockTranslator
