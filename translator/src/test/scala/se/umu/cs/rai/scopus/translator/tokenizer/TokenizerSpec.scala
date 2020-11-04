package se.umu.cs.rai.scopus.translator.tokenizer

import org.scalatest.funspec.AnyFunSpec

import scala.language.implicitConversions

case class TokenizerSpec() extends AnyFunSpec {

  val InputProgram0: String = "val Constant = \"my text\"" +
    "\ndef f(x: Int): Int = x" +
    "\n"

  val ExpectedTokens0: Seq[Token] = Seq(
    Token(InputProgram0, ParserState().PlainWhitespaceState, 0, 0),
    Token(InputProgram0, ParserState().PlainLetterOrDigitOrUnderscoreState, 0, 3),
    Token(InputProgram0, ParserState().PlainWhitespaceState, 3, 4),
    Token(InputProgram0, ParserState().PlainLetterOrDigitOrUnderscoreState, 4, 12),
    Token(InputProgram0, ParserState().PlainWhitespaceState, 12, 13),
    Token(InputProgram0, ParserState().PlainSymbolState, 13, 14),
    Token(InputProgram0, ParserState().PlainWhitespaceState, 14, 15),
    Token(InputProgram0, ParserState().QuotesState, 15, 24),
    Token(InputProgram0, ParserState().PlainWhitespaceState, 24, 25),
    Token(InputProgram0, ParserState().PlainLetterOrDigitOrUnderscoreState, 25, 28),
    Token(InputProgram0, ParserState().PlainWhitespaceState, 28, 29),
    Token(InputProgram0, ParserState().PlainLetterOrDigitOrUnderscoreState, 29, 30),
    Token(InputProgram0, ParserState().PlainSymbolState, 30, 31),
    Token(InputProgram0, ParserState().PlainLetterOrDigitOrUnderscoreState, 31, 32),
    Token(InputProgram0, ParserState().PlainSymbolState, 32, 33),
    Token(InputProgram0, ParserState().PlainWhitespaceState, 33, 34),
    Token(InputProgram0, ParserState().PlainLetterOrDigitOrUnderscoreState, 34, 37),
    Token(InputProgram0, ParserState().PlainSymbolState, 37, 39),
    Token(InputProgram0, ParserState().PlainWhitespaceState, 39, 40),
    Token(InputProgram0, ParserState().PlainLetterOrDigitOrUnderscoreState, 40, 43),
    Token(InputProgram0, ParserState().PlainWhitespaceState, 43, 44),
    Token(InputProgram0, ParserState().PlainSymbolState, 44, 45),
    Token(InputProgram0, ParserState().PlainWhitespaceState, 45, 46),
    Token(InputProgram0, ParserState().PlainLetterOrDigitOrUnderscoreState, 46, 47),
    Token(InputProgram0, ParserState().PlainWhitespaceState, 47, 48))


  it("should tokenize a small example") {
    val obtained = Tokenizer().tokenize(InputProgram0)
    assert(obtained === ExpectedTokens0)
  }

}
