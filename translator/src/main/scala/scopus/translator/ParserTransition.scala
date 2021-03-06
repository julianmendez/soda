package scopus.translator


case class ParserState(index: Int)


case class ParserStateCons() {

  val UndefinedState = ParserState(0)
  val QuotesState = ParserState(1)
  val ApostropheState = ParserState(2)
  val QuotesBackslashState = ParserState(3)
  val ApostropheBackslashState = ParserState(4)
  val Plain = ParserState(5)


  def is_same_class(x: ParserState, y: ParserState): Boolean =
    (x == y) ||
    _is_same_class_with_order(x, y) ||
    _is_same_class_with_order(y, x)

  def _is_same_class_with_order(x: ParserState, y: ParserState): Boolean =
    (x == QuotesState && y == QuotesBackslashState) ||
    (x == ApostropheState && y == ApostropheBackslashState)

}

case class ParserTransition() {

  val ps = ParserStateCons()
  val ch = CharTypeCons()

  def next_parser_state(parser_state: ParserState, char_type: CharType): ParserState =
    TransitionsThatChangeStates.getOrElse((parser_state, char_type), parser_state)

  def TransitionsThatChangeStates: Map[(ParserState, CharType), ParserState] =
    Map(
      /* */
      ((ps.QuotesState, ch.UndefinedType), ps.UndefinedState),      ((ps.QuotesState, ch.QuotesType), ps.Plain),      ((ps.QuotesState, ch.BackslashType), ps.QuotesBackslashState),      /* */
      ((ps.ApostropheState, ch.UndefinedType), ps.UndefinedState),      ((ps.ApostropheState, ch.ApostropheType), ps.Plain),      ((ps.ApostropheState, ch.BackslashType), ps.ApostropheBackslashState),      /* */
      ((ps.QuotesBackslashState, ch.UndefinedType), ps.UndefinedState),      ((ps.QuotesBackslashState, ch.QuotesType), ps.QuotesState),      ((ps.QuotesBackslashState, ch.ApostropheType), ps.QuotesState),      ((ps.QuotesBackslashState, ch.BackslashType), ps.QuotesState),      ((ps.QuotesBackslashState, ch.PlainType), ps.QuotesState),      /* */
      ((ps.ApostropheBackslashState, ch.UndefinedType), ps.UndefinedState),      ((ps.ApostropheBackslashState, ch.QuotesType), ps.ApostropheState),      ((ps.ApostropheBackslashState, ch.ApostropheType), ps.ApostropheState),      ((ps.ApostropheBackslashState, ch.BackslashType), ps.ApostropheState),      ((ps.ApostropheBackslashState, ch.PlainType), ps.ApostropheState),      /* */
      ((ps.Plain, ch.UndefinedType), ps.UndefinedState),      ((ps.Plain, ch.QuotesType), ps.QuotesState),      ((ps.Plain, ch.ApostropheType), ps.ApostropheState),      ((ps.Plain, ch.BackslashType), ps.Plain),      ((ps.Plain, ch.PlainType), ps.Plain)
    )

}