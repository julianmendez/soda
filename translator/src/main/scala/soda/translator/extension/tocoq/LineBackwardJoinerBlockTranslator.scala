package soda.translator.extension.tocoq

trait LineBackwardJoinerBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block

  lazy val source = "soda"

  lazy val target = "soda"

  lazy val mtr = soda.translator.extension.toscala.MicroTranslatorToScala_ ()

  def translate (block: Block ): Block =
    mtr.join_lines_with_backward_join (block )

}

case class LineBackwardJoinerBlockTranslator_ ()  extends LineBackwardJoinerBlockTranslator
