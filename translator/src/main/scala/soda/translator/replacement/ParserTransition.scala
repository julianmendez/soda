package soda.translator.replacement


/**
 * This models all the possible states that the parser can be.
 */
case class ParserState (ordinal: Int, name: String ) extends soda.lib.EnumConstant

trait ParserStateConstant {

  lazy val UndefinedState = ParserState (0, "UndefinedState")
  lazy val QuotesState = ParserState (1, "QuotesState")
  lazy val ApostropheState = ParserState (2, "ApostropheState")
  lazy val QuotesBackslashState = ParserState (3, "QuotesBackslashState")
  lazy val ApostropheBackslashState = ParserState (4, "ApostropheBackslashState")
  lazy val Plain = ParserState (5, "Plain")

  lazy val ParserStateValues = Seq (UndefinedState, QuotesState, ApostropheState, QuotesBackslashState, ApostropheBackslashState, Plain )
}

/**
 * This is an enumeration of all the parser states.
 */
trait ParserStateEnum extends ParserStateConstant {

  lazy val values = ParserStateValues

  def is_same_class (x: ParserState, y: ParserState ): Boolean =
    (x == y ) || is_like (x, y ) || is_like (y, x )

  def is_like (x: ParserState, y: ParserState ): Boolean =
    (x == QuotesState && y == QuotesBackslashState ) ||
      (x == ApostropheState && y == ApostropheBackslashState )
}

case class ParserStateEnumImpl () extends ParserStateEnum

trait ParserTransition extends ParserStateConstant with CharTypeConstant {

  lazy val TransitionsThatChangeStates: Map [(ParserState, CharType ), ParserState] =
    Map (/* */
      ((QuotesState, UndefinedType ), UndefinedState ), ((QuotesState, QuotesType ), Plain ), ((QuotesState, BackslashType ), QuotesBackslashState ), /* */
      ((ApostropheState, UndefinedType ), UndefinedState ), ((ApostropheState, ApostropheType ), Plain ), ((ApostropheState, BackslashType ), ApostropheBackslashState ), /* */
      ((QuotesBackslashState, UndefinedType ), UndefinedState ), ((QuotesBackslashState, QuotesType ), QuotesState ), ((QuotesBackslashState, ApostropheType ), QuotesState ), ((QuotesBackslashState, BackslashType ), QuotesState ), ((QuotesBackslashState, PlainType ), QuotesState ), /* */
      ((ApostropheBackslashState, UndefinedType ), UndefinedState ), ((ApostropheBackslashState, QuotesType ), ApostropheState ), ((ApostropheBackslashState, ApostropheType ), ApostropheState ), ((ApostropheBackslashState, BackslashType ), ApostropheState ), ((ApostropheBackslashState, PlainType ), ApostropheState ), /* */
      ((Plain, UndefinedType ), UndefinedState ), ((Plain, QuotesType ), QuotesState ), ((Plain, ApostropheType ), ApostropheState ), ((Plain, BackslashType ), Plain ), ((Plain, PlainType ), Plain )    )

  def next_parser_state (parser_state: ParserState, char_type: CharType ): ParserState =
    TransitionsThatChangeStates.getOrElse ((parser_state, char_type ), parser_state )
}

case class ParserTransitionImpl () extends ParserTransition
