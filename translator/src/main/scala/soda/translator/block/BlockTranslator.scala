package soda.translator.block

trait BlockTranslator {

  def translate (block: Block ): Block

}

case class DefaultBlockTranslator_ ()  extends BlockTranslator {

  def translate (block: Block ): Block =
    block

}
