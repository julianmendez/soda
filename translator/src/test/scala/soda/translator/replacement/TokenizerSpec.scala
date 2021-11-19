package soda.translator.replacement

case class TokenizerSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("should tokenize a small example")
    {
      lazy val input = "    val Constant = \"my text\""
      lazy val expected = Seq (Token_ ("    val Constant = ", ParserStateEnum_ () .plain, 0 ), Token_ ("\"my text\"", ParserStateEnum_ () .quotes_state, 19 ), Token_ ("", ParserStateEnum_ () .plain, 28 )      )
      lazy val obtained = Tokenizer_ (input ) .tokens
      assert (obtained == expected ) }

  test ("should tokenize a common tab in a string")
    {
      lazy val input = "  x = \"abc\tde\""
      lazy val expected = Seq (Token_ ("  x = ", ParserStateEnum_ () .plain, 0 ), Token_ ("\"abc\tde\"", ParserStateEnum_ () .quotes_state, 6 ), Token_ ("", ParserStateEnum_ () .plain, 14 )      )
      lazy val obtained = Tokenizer_ (input ) .tokens
      assert (obtained == expected ) }

  test ("should tokenize an escaped tab in a string")
    {
      lazy val input = "  x = \"abc\\tde\""
      lazy val expected = Seq (Token_ ("  x = ", ParserStateEnum_ () .plain, 0 ), Token_ ("\"abc\\tde\"", ParserStateEnum_ () .quotes_state, 6 ), Token_ ("", ParserStateEnum_ () .plain, 15 )      )
      lazy val obtained = Tokenizer_ (input ) .tokens
      assert (obtained == expected ) }

  test ("should tokenize a single function definition")
    {
      lazy val input = "def f(x: Int): Int = x"
      lazy val expected = Seq (Token_ ("def f(x: Int): Int = x", ParserStateEnum_ () .plain, 0 )      )
      lazy val obtained = Tokenizer_ (input ) .tokens
      assert (obtained == expected ) }

  test ("should tokenize a function call")
    {
      lazy val input = "\tas_digits (5 * number)"
      lazy val expected = Seq (Token_ ("\tas_digits (5 * number)", ParserStateEnum_ () .plain, 0 )      )
      lazy val obtained = Tokenizer_ (input ) .tokens
      assert (obtained == expected ) }

}
