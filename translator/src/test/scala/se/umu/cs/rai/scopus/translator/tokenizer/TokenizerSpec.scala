package se.umu.cs.rai.scopus.translator.tokenizer

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

case class TokenizerSpec() extends AnyFunSpec {


  val InputLine0: String = "val Constant = \"my text\""
  val InputLine1: String = "def f(x: Int): Int = x"


  val ExpectedTokens0: Seq[Token] = Seq(
    Token("val Constant = ", ParserState().Plain),
    Token("\"my text\"", ParserState().QuotesState),
    Token("", ParserState().Plain)
  )

  val ExpectedTokens1: Seq[Token] = Seq(
    Token("def f(x: Int): Int = x", ParserState().Plain)
  )


  it("should tokenize a small example") {
    val obtained0 = Tokenizer().tokenize(InputLine0)
    assert(obtained0 === ExpectedTokens0)

    val obtained1 = Tokenizer().tokenize(InputLine1)
    assert(obtained1 === ExpectedTokens1)
  }

}
