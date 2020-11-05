package se.umu.cs.rai.scopus.translator

case class ParserState() {
  val UndefinedState = 0
  val QuotesState = 1
  val ApostropheState = 2
  val QuotesBackslashState = 3
  val ApostropheBackslashState = 4
  val Plain = 5
}

case class ParserTransition() {

  val TransitionsThatChangeStates: Map[(Int, Int), Int] =
    Map(
      ((ParserState().QuotesState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().QuotesState, CharType().QuotesType), ParserState().Plain),
      ((ParserState().QuotesState, CharType().BackslashType), ParserState().QuotesBackslashState),

      ((ParserState().ApostropheState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().ApostropheState, CharType().ApostropheType), ParserState().Plain),
      ((ParserState().ApostropheState, CharType().BackslashType), ParserState().ApostropheBackslashState),

      ((ParserState().QuotesBackslashState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().QuotesBackslashState, CharType().QuotesType), ParserState().QuotesState),
      ((ParserState().QuotesBackslashState, CharType().ApostropheType), ParserState().QuotesState),
      ((ParserState().QuotesBackslashState, CharType().BackslashType), ParserState().QuotesState),
      ((ParserState().QuotesBackslashState, CharType().PlainType), ParserState().QuotesState),

      ((ParserState().ApostropheBackslashState, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().ApostropheBackslashState, CharType().QuotesType), ParserState().ApostropheState),
      ((ParserState().ApostropheBackslashState, CharType().ApostropheType), ParserState().ApostropheState),
      ((ParserState().ApostropheBackslashState, CharType().BackslashType), ParserState().ApostropheState),
      ((ParserState().ApostropheBackslashState, CharType().PlainType), ParserState().ApostropheState),

      ((ParserState().Plain, CharType().UndefinedType), ParserState().UndefinedState),
      ((ParserState().Plain, CharType().QuotesType), ParserState().QuotesState),
      ((ParserState().Plain, CharType().ApostropheType), ParserState().ApostropheState),
      ((ParserState().Plain, CharType().BackslashType), ParserState().Plain),
      ((ParserState().Plain, CharType().PlainType), ParserState().Plain),
    )

  def nextParserState(parserState: Int, charType: Int): Int =
    TransitionsThatChangeStates.getOrElse((parserState, charType), parserState)

}
