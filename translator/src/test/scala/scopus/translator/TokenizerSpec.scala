package scopus.translator

import org.scalatest.funsuite.AnyFunSuite


case class TokenizerSpec() extends AnyFunSuite {

  test("should tokenize a small example") {
    val input = "    val Constant = \"my text\""
    val expected = Seq(
      Token("    val Constant = ", ParserStateEnum().Plain, 0),      Token("\"my text\"", ParserStateEnum().QuotesState, 19),      Token("", ParserStateEnum().Plain, 28)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize a common tab in a string") {
    val input = "  x = \"abc\tde\""
    val expected = Seq(
      Token("  x = ", ParserStateEnum().Plain, 0),      Token("\"abc\tde\"", ParserStateEnum().QuotesState, 6),      Token("", ParserStateEnum().Plain, 14)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize an escaped tab in a string") {
    val input = "  x = \"abc\\tde\""
    val expected = Seq(
      Token("  x = ", ParserStateEnum().Plain, 0),      Token("\"abc\\tde\"", ParserStateEnum().QuotesState, 6),      Token("", ParserStateEnum().Plain, 15)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize a single function definition") {
    val input = "def f(x: Int): Int = x"
    val expected = Seq(
      Token("def f(x: Int): Int = x", ParserStateEnum().Plain, 0)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }

  test("should tokenize a function call") {
    val input = "\tas_digits (5 * number)"
    val expected = Seq(
      Token("\tas_digits (5 * number)", ParserStateEnum().Plain, 0)
    )
    val obtained = Tokenizer().tokenize(input)
    assert(obtained == expected)
  }


}
