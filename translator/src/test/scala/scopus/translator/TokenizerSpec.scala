package scopus.translator

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions


case class TokenizerSpec() extends AnyFunSuite {

  test("should tokenize a small example") {
    val input = "    val Constant = \"my text\""
    val expected = Seq(
      Token("    val Constant = ", ParserState().Plain, 0),      Token("\"my text\"", ParserState().QuotesState, 19),      Token("", ParserState().Plain, 28)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize a common tab in a string") {
    val input = "  x = \"abc\tde\""
    val expected = Seq(
      Token("  x = ", ParserState().Plain, 0),      Token("\"abc\tde\"", ParserState().QuotesState, 6),      Token("", ParserState().Plain, 14)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize an escaped tab in a string") {
    val input = "  x = \"abc\\tde\""
    val expected = Seq(
      Token("  x = ", ParserState().Plain, 0),      Token("\"abc\\tde\"", ParserState().QuotesState, 6),      Token("", ParserState().Plain, 15)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize a single function definition") {
    val input = "def f(x: Int): Int = x"
    val expected = Seq(
      Token("def f(x: Int): Int = x", ParserState().Plain, 0)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize a function call") {
    val input = "\tas_digits (5 * number)"
    val expected = Seq(
      Token("\tas_digits (5 * number)", ParserState().Plain, 0)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }


}
