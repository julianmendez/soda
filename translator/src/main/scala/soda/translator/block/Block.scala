package soda.translator.block

trait Block {

  def lines: Seq [String]

  lazy val new_line = "\n"

  lazy val contents: String =
    lines.mkString (new_line )

}

case class Block_ (lines: Seq [String]  )  extends Block
