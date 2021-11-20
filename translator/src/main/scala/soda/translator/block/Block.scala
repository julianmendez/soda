package soda.translator.block

trait Block {

  def contents: String

}

case class Block_ (contents: String )  extends Block

trait BlockTranslator {

  def source: String

  def target: String

  def translate (block: Block ): Block

}
