package soda.translator.replacement


case class TokenizerSpec () extends org.scalatest.funsuite.AnyFunSuite {

  test ("should tokenize a small example") {
    lazy val input = "    val Constant = \"my text\""
    lazy val expected = Seq (Token ("    val Constant = ", ParserStateEnum () .Plain, 0 ), Token ("\"my text\"", ParserStateEnum () .QuotesState, 19 ), Token ("", ParserStateEnum () .Plain, 28 )
    )
    lazy val obtained = Tokenizer (input ) .get_tokens
    assert (obtained == expected )
  }

  test ("should tokenize a common tab in a string") {
    lazy val input = "  x = \"abc\tde\""
    lazy val expected = Seq (Token ("  x = ", ParserStateEnum () .Plain, 0 ), Token ("\"abc\tde\"", ParserStateEnum () .QuotesState, 6 ), Token ("", ParserStateEnum () .Plain, 14 )
    )
    lazy val obtained = Tokenizer (input ) .get_tokens
    assert (obtained == expected )
  }

  test ("should tokenize an escaped tab in a string") {
    lazy val input = "  x = \"abc\\tde\""
    lazy val expected = Seq (Token ("  x = ", ParserStateEnum () .Plain, 0 ), Token ("\"abc\\tde\"", ParserStateEnum () .QuotesState, 6 ), Token ("", ParserStateEnum () .Plain, 15 )
    )
    lazy val obtained = Tokenizer (input ) .get_tokens
    assert (obtained == expected )
  }

  test ("should tokenize a single function definition") {
    lazy val input = "def f(x: Int): Int = x"
    lazy val expected = Seq (Token ("def f(x: Int): Int = x", ParserStateEnum () .Plain, 0 )
    )
    lazy val obtained = Tokenizer (input ) .get_tokens
    assert (obtained == expected )
  }

  test ("should tokenize a function call") {
    lazy val input = "\tas_digits (5 * number)"
    lazy val expected = Seq (Token ("\tas_digits (5 * number)", ParserStateEnum () .Plain, 0 )
    )
    lazy val obtained = Tokenizer (input ) .get_tokens
    assert (obtained == expected )
  }

}
