package soda.translator.replacement


case class TokenizerSpec () extends org.scalatest.funsuite.AnyFunSuite {

  test ("should tokenize a small example") {
    lazy val input = "    val Constant = \"my text\""
    lazy val expected = Seq (Token ("    val Constant = ", ParserStateEnum () .plain, 0 ), Token ("\"my text\"", ParserStateEnum () .quotes_state, 19 ), Token ("", ParserStateEnum () .plain, 28 )    )
    lazy val obtained = TokenizerImpl (input ) .tokens

    assert (obtained == expected )
  }

  test ("should tokenize a common tab in a string") {
    lazy val input = "  x = \"abc\tde\""
    lazy val expected = Seq (Token ("  x = ", ParserStateEnum () .plain, 0 ), Token ("\"abc\tde\"", ParserStateEnum () .quotes_state, 6 ), Token ("", ParserStateEnum () .plain, 14 )    )
    lazy val obtained = TokenizerImpl (input ) .tokens

    assert (obtained == expected )
  }

  test ("should tokenize an escaped tab in a string") {
    lazy val input = "  x = \"abc\\tde\""
    lazy val expected = Seq (Token ("  x = ", ParserStateEnum () .plain, 0 ), Token ("\"abc\\tde\"", ParserStateEnum () .quotes_state, 6 ), Token ("", ParserStateEnum () .plain, 15 )    )
    lazy val obtained = TokenizerImpl (input ) .tokens

    assert (obtained == expected )
  }

  test ("should tokenize a single function definition") {
    lazy val input = "def f(x: Int): Int = x"
    lazy val expected = Seq (Token ("def f(x: Int): Int = x", ParserStateEnum () .plain, 0 )    )
    lazy val obtained = TokenizerImpl (input ) .tokens

    assert (obtained == expected )
  }

  test ("should tokenize a function call") {
    lazy val input = "\tas_digits (5 * number)"
    lazy val expected = Seq (Token ("\tas_digits (5 * number)", ParserStateEnum () .plain, 0 )    )
    lazy val obtained = TokenizerImpl (input ) .tokens

    assert (obtained == expected )
  }
}
