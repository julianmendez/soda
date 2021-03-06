package scopus.translator


case class ParserState() {
  val UndefinedState = 0
  val QuotesState = 1
  val ApostropheState = 2
  val QuotesBackslashState = 3
  val ApostropheBackslashState = 4
  val Plain = 5


  def is_same_class(x: Int, y: Int): Boolean =
    (x == y) ||
    _is_same_class_with_order(x, y) ||
    _is_same_class_with_order(y, x)

  def _is_same_class_with_order(x: Int, y: Int): Boolean =
    (x == QuotesState && y == QuotesBackslashState) ||
    (x == ApostropheState && y == ApostropheBackslashState)

}

case class ParserTransition() {

  val ps = ParserState()
  val ch = CharTypeCons()

  def next_parser_state(parser_state: Int, char_type: CharType): Int =
    TransitionsThatChangeStates.getOrElse((parser_state, char_type), parser_state)

  def TransitionsThatChangeStates: Map[(Int, CharType), Int] =
    Map(
      /* */
      ((ps.QuotesState, ch.UndefinedType), ps.UndefinedState),      ((ps.QuotesState, ch.QuotesType), ps.Plain),      ((ps.QuotesState, ch.BackslashType), ps.QuotesBackslashState),      /* */
      ((ps.ApostropheState, ch.UndefinedType), ps.UndefinedState),      ((ps.ApostropheState, ch.ApostropheType), ps.Plain),      ((ps.ApostropheState, ch.BackslashType), ps.ApostropheBackslashState),      /* */
      ((ps.QuotesBackslashState, ch.UndefinedType), ps.UndefinedState),      ((ps.QuotesBackslashState, ch.QuotesType), ps.QuotesState),      ((ps.QuotesBackslashState, ch.ApostropheType), ps.QuotesState),      ((ps.QuotesBackslashState, ch.BackslashType), ps.QuotesState),      ((ps.QuotesBackslashState, ch.PlainType), ps.QuotesState),      /* */
      ((ps.ApostropheBackslashState, ch.UndefinedType), ps.UndefinedState),      ((ps.ApostropheBackslashState, ch.QuotesType), ps.ApostropheState),      ((ps.ApostropheBackslashState, ch.ApostropheType), ps.ApostropheState),      ((ps.ApostropheBackslashState, ch.BackslashType), ps.ApostropheState),      ((ps.ApostropheBackslashState, ch.PlainType), ps.ApostropheState),      /* */
      ((ps.Plain, ch.UndefinedType), ps.UndefinedState),      ((ps.Plain, ch.QuotesType), ps.QuotesState),      ((ps.Plain, ch.ApostropheType), ps.ApostropheState),      ((ps.Plain, ch.BackslashType), ps.Plain),      ((ps.Plain, ch.PlainType), ps.Plain)
    )

}
