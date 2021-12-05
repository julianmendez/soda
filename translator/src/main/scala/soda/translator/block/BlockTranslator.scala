package soda.translator.block

trait BlockTranslator {

  def source: String

  def target: String

  def translate (block: Block ): Block

}

case class DefaultBlockTranslator_ ()  extends BlockTranslator {

  lazy val source = "default"

  lazy val target = "default"

  def translate (block: Block ): Block =
    block

}
