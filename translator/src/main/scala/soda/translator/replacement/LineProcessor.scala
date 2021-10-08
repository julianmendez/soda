package soda.translator.replacement


trait SingleLineProcessor {

  def line: String
}

trait MultiLineProcessor {

  def lines: Seq [String]
}
