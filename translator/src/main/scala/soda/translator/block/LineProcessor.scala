package soda.translator.block

trait SingleLineProcessor {

  def line: String

}

trait MultiLineProcessor {

  def lines: Seq [String]

}
