package soda.translator.block

trait PlainBlock {

  def lines: Seq [String]

  lazy val new_line = "\n"

  lazy val contents: String =
    lines.mkString (new_line )

}

case class PlainBlock_ (lines: Seq [String]  )  extends PlainBlock
