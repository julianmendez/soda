package soda.translator.extension.tocoq

trait LineForwardJoinerBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block

  lazy val source = "soda"

  lazy val target = "soda"

  lazy val mtr = soda.translator.extension.toscala.MicroTranslatorToScala_ ()

  def translate (block: Block ): Block =
    mtr.join_lines_with_forward_join (block )

}

case class LineForwardJoinerBlockTranslator_ ()  extends LineForwardJoinerBlockTranslator
