package scopus.translator

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions


case class TokenizerSpec() extends AnyFunSuite {

  test("should tokenize a small example") {
    val input = "    val Constant = \"my text\""
    val expected = Seq(
      Token("    val Constant = ", ParserStateCons().Plain, 0),      Token("\"my text\"", ParserStateCons().QuotesState, 19),      Token("", ParserStateCons().Plain, 28)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize a common tab in a string") {
    val input = "  x = \"abc\tde\""
    val expected = Seq(
      Token("  x = ", ParserStateCons().Plain, 0),      Token("\"abc\tde\"", ParserStateCons().QuotesState, 6),      Token("", ParserStateCons().Plain, 14)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize an escaped tab in a string") {
    val input = "  x = \"abc\\tde\""
    val expected = Seq(
      Token("  x = ", ParserStateCons().Plain, 0),      Token("\"abc\\tde\"", ParserStateCons().QuotesState, 6),      Token("", ParserStateCons().Plain, 15)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize a single function definition") {
    val input = "def f(x: Int): Int = x"
    val expected = Seq(
      Token("def f(x: Int): Int = x", ParserStateCons().Plain, 0)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize a function call") {
    val input = "\tas_digits (5 * number)"
    val expected = Seq(
      Token("\tas_digits (5 * number)", ParserStateCons().Plain, 0)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }


}
