package soda.translator.replacement


/**
 * This models all the possible states that the parser can be.
 */
case class ParserState (ordinal: Int, name: String ) extends soda.lib.EnumConstant

/**
 * This is an enumeration of all the parser states.
 */
case class ParserStateEnum () {

  lazy val UndefinedState = ParserState (0, "UndefinedState")
  lazy val QuotesState = ParserState (1, "QuotesState")
  lazy val ApostropheState = ParserState (2, "ApostropheState")
  lazy val QuotesBackslashState = ParserState (3, "QuotesBackslashState")
  lazy val ApostropheBackslashState = ParserState (4, "ApostropheBackslashState")
  lazy val Plain = ParserState (5, "Plain")

  lazy val values = Seq (UndefinedState, QuotesState, ApostropheState, QuotesBackslashState, ApostropheBackslashState, Plain )

  def is_same_class (x: ParserState, y: ParserState ): Boolean =
    (x == y ) || is_like (x, y ) || is_like (y, x )

  def is_like (x: ParserState, y: ParserState ): Boolean =
    (x == QuotesState && y == QuotesBackslashState ) ||
      (x == ApostropheState && y == ApostropheBackslashState )
}

case class ParserTransition () {

  lazy val ps = ParserStateEnum ()
  lazy val ch = CharTypeEnum ()

  lazy val TransitionsThatChangeStates: Map [(ParserState, CharType ), ParserState] =
    Map (/* */
      ((ps.QuotesState, ch.UndefinedType ), ps.UndefinedState ), ((ps.QuotesState, ch.QuotesType ), ps.Plain ), ((ps.QuotesState, ch.BackslashType ), ps.QuotesBackslashState ), /* */
      ((ps.ApostropheState, ch.UndefinedType ), ps.UndefinedState ), ((ps.ApostropheState, ch.ApostropheType ), ps.Plain ), ((ps.ApostropheState, ch.BackslashType ), ps.ApostropheBackslashState ), /* */
      ((ps.QuotesBackslashState, ch.UndefinedType ), ps.UndefinedState ), ((ps.QuotesBackslashState, ch.QuotesType ), ps.QuotesState ), ((ps.QuotesBackslashState, ch.ApostropheType ), ps.QuotesState ), ((ps.QuotesBackslashState, ch.BackslashType ), ps.QuotesState ), ((ps.QuotesBackslashState, ch.PlainType ), ps.QuotesState ), /* */
      ((ps.ApostropheBackslashState, ch.UndefinedType ), ps.UndefinedState ), ((ps.ApostropheBackslashState, ch.QuotesType ), ps.ApostropheState ), ((ps.ApostropheBackslashState, ch.ApostropheType ), ps.ApostropheState ), ((ps.ApostropheBackslashState, ch.BackslashType ), ps.ApostropheState ), ((ps.ApostropheBackslashState, ch.PlainType ), ps.ApostropheState ), /* */
      ((ps.Plain, ch.UndefinedType ), ps.UndefinedState ), ((ps.Plain, ch.QuotesType ), ps.QuotesState ), ((ps.Plain, ch.ApostropheType ), ps.ApostropheState ), ((ps.Plain, ch.BackslashType ), ps.Plain ), ((ps.Plain, ch.PlainType ), ps.Plain )    )

  def next_parser_state (parser_state: ParserState, char_type: CharType ): ParserState =
    TransitionsThatChangeStates.getOrElse ((parser_state, char_type ), parser_state )
}
