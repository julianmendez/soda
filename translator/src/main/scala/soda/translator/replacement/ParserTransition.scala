package soda.translator.replacement


/**
 * This models all the possible states that the parser can be.
 */
trait ParserState extends soda.lib.EnumConstant

case class ParserState_ (ordinal: Int, name: String ) extends ParserState

trait ParserStateConstant {

  lazy val undefined_state = ParserState_ (0, "undefined_state")

  lazy val quotes_state = ParserState_ (1, "quotes_state")

  lazy val apostrophe_state = ParserState_ (2, "apostrophe_state")

  lazy val quotes_backslash_state = ParserState_ (3, "quotes_backslash_state")

  lazy val apostrophe_backslash_state = ParserState_ (4, "apostrophe_backslash_state")

  lazy val plain = ParserState_ (5, "plain")

  lazy val parser_state_values = Seq (undefined_state, quotes_state, apostrophe_state, quotes_backslash_state, apostrophe_backslash_state, plain )
}

/**
 * This is an enumeration of all the parser states.
 */
trait ParserStateEnum extends ParserStateConstant {

  lazy val values = parser_state_values

  def is_same_class (x: ParserState, y: ParserState ): Boolean =
    (x == y ) || is_like (x, y ) || is_like (y, x )

  def is_like (x: ParserState, y: ParserState ): Boolean =
    (x == quotes_state && y == quotes_backslash_state ) ||
      (x == apostrophe_state && y == apostrophe_backslash_state )
}

case class ParserStateEnum_ () extends ParserStateEnum

trait ParserTransition  extends ParserStateConstant  with CharTypeConstant {

  lazy val transitions_that_change_states: Map [(ParserState, CharType ), ParserState] =
    Map (/* */
      ((quotes_state, undefined_type ), undefined_state ), ((quotes_state, quotes_type ), plain ), ((quotes_state, backslash_type ), quotes_backslash_state ), /* */
      ((apostrophe_state, undefined_type ), undefined_state ), ((apostrophe_state, apostrophe_type ), plain ), ((apostrophe_state, backslash_type ), apostrophe_backslash_state ), /* */
      ((quotes_backslash_state, undefined_type ), undefined_state ), ((quotes_backslash_state, quotes_type ), quotes_state ), ((quotes_backslash_state, apostrophe_type ), quotes_state ), ((quotes_backslash_state, backslash_type ), quotes_state ), ((quotes_backslash_state, plain_type ), quotes_state ), /* */
      ((apostrophe_backslash_state, undefined_type ), undefined_state ), ((apostrophe_backslash_state, quotes_type ), apostrophe_state ), ((apostrophe_backslash_state, apostrophe_type ), apostrophe_state ), ((apostrophe_backslash_state, backslash_type ), apostrophe_state ), ((apostrophe_backslash_state, plain_type ), apostrophe_state ), /* */
      ((plain, undefined_type ), undefined_state ), ((plain, quotes_type ), quotes_state ), ((plain, apostrophe_type ), apostrophe_state ), ((plain, backslash_type ), plain ), ((plain, plain_type ), plain )    )

  def next_parser_state (parser_state: ParserState, char_type: CharType ): ParserState =
    transitions_that_change_states.getOrElse ((parser_state, char_type ), parser_state )
}

case class ParserTransition_ () extends ParserTransition
