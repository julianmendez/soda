package se.umu.cs.rai.scopus.translator

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions


case class TokenizerSpec() extends AnyFunSuite {


  val InputLine0: String = "val Constant = \"my text\""
  val InputLine1: String = "def f(x: Int): Int = x"
  val InputLine2: String = "as_digits (5 * number)"


  val ExpectedTokens0: Seq[Token] = Seq(
    Token("val Constant = ", ParserState().Plain),
    Token("\"my text\"", ParserState().QuotesState),
    Token("", ParserState().Plain)
  )

  val ExpectedTokens1: Seq[Token] = Seq(
    Token("def f(x: Int): Int = x", ParserState().Plain)
  )

  val ExpectedTokens2: Seq[Token] = Seq(
    Token("as_digits (5 * number)", ParserState().Plain)
  )


  test("should tokenize a small example") {
    val obtained0 = Tokenizer().tokenize(InputLine0)
    assert(obtained0 === ExpectedTokens0)

    val obtained1 = Tokenizer().tokenize(InputLine1)
    assert(obtained1 === ExpectedTokens1)

    val obtained2 = Tokenizer().tokenize(InputLine2)
    assert(obtained2 === ExpectedTokens2)
  }

}
