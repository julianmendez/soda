package se.umu.cs.rai.scopus.translator.tokenizer

case class ClassifiedTokenType() {
  val Undefined = 0
  val Keyword = 1
  val PredefinedOperation = 2
  val Operator = 3
  val Parentheses = 4
  val Text = 5
}

