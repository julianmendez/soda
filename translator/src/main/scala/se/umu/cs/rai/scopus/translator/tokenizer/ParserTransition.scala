package se.umu.cs.rai.scopus.translator.tokenizer

case class ParserState() {
  val UndefinedState = 0
  val QuotesState = 1
  val ApostropheState = 2
  val QuotesBackslashState = 3
  val ApostropheBackslashState = 4
  val PlainWhitespaceState = 5
  val PlainLetterOrDigitOrUnderscoreState = 6
  val PlainSymbolState = 7
}

case class ParserTransition() {

  val TransitionsThatChangeStates: Map[(Int, Int), Int] =
    Map(
      ((ParserState().QuotesState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().QuotesState, CharType().QuotesType), ParserState().PlainWhitespaceState),
      ((ParserState().QuotesState, CharType().BackslashType), ParserState().QuotesBackslashState),

      ((ParserState().ApostropheState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().ApostropheState, CharType().ApostropheType), ParserState().PlainWhitespaceState),
      ((ParserState().ApostropheState, CharType().BackslashType), ParserState().ApostropheBackslashState),

      ((ParserState().QuotesBackslashState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().QuotesBackslashState, CharType().WhitespaceType), ParserState().QuotesState),
      ((ParserState().QuotesBackslashState, CharType().QuotesType), ParserState().QuotesState),
      ((ParserState().QuotesBackslashState, CharType().ApostropheType), ParserState().QuotesState),
      ((ParserState().QuotesBackslashState, CharType().BackslashType), ParserState().QuotesState),
      ((ParserState().QuotesBackslashState, CharType().LetterOrDigitOrUnderscoreType), ParserState().QuotesState),
      ((ParserState().QuotesBackslashState, CharType().SymbolType), ParserState().QuotesState),

      ((ParserState().ApostropheBackslashState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().ApostropheBackslashState, CharType().WhitespaceType), ParserState().ApostropheState),
      ((ParserState().ApostropheBackslashState, CharType().QuotesType), ParserState().ApostropheState),
      ((ParserState().ApostropheBackslashState, CharType().ApostropheType), ParserState().ApostropheState),
      ((ParserState().ApostropheBackslashState, CharType().BackslashType), ParserState().ApostropheState),
      ((ParserState().ApostropheBackslashState, CharType().LetterOrDigitOrUnderscoreType), ParserState().ApostropheState),
      ((ParserState().ApostropheBackslashState, CharType().SymbolType), ParserState().ApostropheState),

      ((ParserState().PlainWhitespaceState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().PlainWhitespaceState, CharType().WhitespaceType), ParserState().PlainWhitespaceState),
      ((ParserState().PlainWhitespaceState, CharType().QuotesType), ParserState().QuotesState),
      ((ParserState().PlainWhitespaceState, CharType().ApostropheType), ParserState().ApostropheState),
      ((ParserState().PlainWhitespaceState, CharType().BackslashType), ParserState().PlainWhitespaceState),
      ((ParserState().PlainWhitespaceState, CharType().LetterOrDigitOrUnderscoreType), ParserState().PlainLetterOrDigitOrUnderscoreState),
      ((ParserState().PlainWhitespaceState, CharType().SymbolType), ParserState().PlainSymbolState),

      ((ParserState().PlainLetterOrDigitOrUnderscoreState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().PlainLetterOrDigitOrUnderscoreState, CharType().WhitespaceType), ParserState().PlainWhitespaceState),
      ((ParserState().PlainLetterOrDigitOrUnderscoreState, CharType().QuotesType), ParserState().QuotesState),
      ((ParserState().PlainLetterOrDigitOrUnderscoreState, CharType().ApostropheType), ParserState().ApostropheState),
      ((ParserState().PlainLetterOrDigitOrUnderscoreState, CharType().BackslashType), ParserState().PlainWhitespaceState),
      ((ParserState().PlainLetterOrDigitOrUnderscoreState, CharType().LetterOrDigitOrUnderscoreType), ParserState().PlainLetterOrDigitOrUnderscoreState),
      ((ParserState().PlainLetterOrDigitOrUnderscoreState, CharType().SymbolType), ParserState().PlainSymbolState),

      ((ParserState().PlainSymbolState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().PlainSymbolState, CharType().WhitespaceType), ParserState().PlainWhitespaceState),
      ((ParserState().PlainSymbolState, CharType().QuotesType), ParserState().QuotesState),
      ((ParserState().PlainSymbolState, CharType().ApostropheType), ParserState().ApostropheState),
      ((ParserState().PlainSymbolState, CharType().BackslashType), ParserState().PlainWhitespaceState),
      ((ParserState().PlainSymbolState, CharType().LetterOrDigitOrUnderscoreType), ParserState().PlainLetterOrDigitOrUnderscoreState),
      ((ParserState().PlainSymbolState, CharType().SymbolType), ParserState().PlainSymbolState)
    )

  def nextParserState(parserState: Int, charType: Int): Int =
    TransitionsThatChangeStates.getOrElse((parserState, charType), parserState)

}
