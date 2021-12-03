package soda.translator.block

trait Block {

  def lines: Seq [String]

  lazy val new_line = "\n"

  lazy val contents: String =
    lines.mkString (new_line )

}

case class Block_ (lines: Seq [String]  )  extends Block

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
