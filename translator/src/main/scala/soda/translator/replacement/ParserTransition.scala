package soda.translator.replacement

/**
 * This models all the possible states that the parser can be.
 */

trait ParserState
  extends
    soda.lib.EnumConstant
{

  def   ordinal: Int
  def   name: String

}

case class ParserState_ (ordinal: Int, name: String )
  extends
    ParserState
{

}

/**
 * This is an enumeration of all the parser states.
 */

trait ParserStateEnum
  extends
    soda.lib.Enum [ParserState]
{

  lazy val undefined_state = ParserState_ (0, "undefined_state")

  lazy val quotes_state = ParserState_ (1, "quotes_state")

  lazy val apostrophe_state = ParserState_ (2, "apostrophe_state")

  lazy val quotes_backslash_state = ParserState_ (3, "quotes_backslash_state")

  lazy val apostrophe_backslash_state = ParserState_ (4, "apostrophe_backslash_state")

  lazy val plain = ParserState_ (5, "plain")

  lazy val values = Seq (undefined_state, quotes_state, apostrophe_state, quotes_backslash_state, apostrophe_backslash_state, plain )

  def is_same_class (x: ParserState, y: ParserState ): Boolean =
     (x == y ) || is_like (x, y ) || is_like (y, x )

  def is_like (x: ParserState, y: ParserState ): Boolean =
     (x == quotes_state && y == quotes_backslash_state ) ||
       (x == apostrophe_state && y == apostrophe_backslash_state )

}

case class ParserStateEnum_ ()
  extends
    ParserStateEnum
{

}

trait ParserTransition
{

  lazy val ps = ParserStateEnum_ ()

  lazy val ct = CharTypeEnum_ ()

  lazy val transitions_that_change_states: Map [(ParserState, CharType ), ParserState] =
    Map (
      /* */
      ((ps.quotes_state, ct.undefined_type ), ps.undefined_state ),
      ((ps.quotes_state, ct.quotes_type ), ps.plain ),
      ((ps.quotes_state, ct.backslash_type ), ps.quotes_backslash_state ),
      /* */
      ((ps.apostrophe_state, ct.undefined_type ), ps.undefined_state ),
      ((ps.apostrophe_state, ct.apostrophe_type ), ps.plain ),
      ((ps.apostrophe_state, ct.backslash_type ), ps.apostrophe_backslash_state ),
      /* */
      ((ps.quotes_backslash_state, ct.undefined_type ), ps.undefined_state ),
      ((ps.quotes_backslash_state, ct.quotes_type ), ps.quotes_state ),
      ((ps.quotes_backslash_state, ct.apostrophe_type ), ps.quotes_state ),
      ((ps.quotes_backslash_state, ct.backslash_type ), ps.quotes_state ),
      ((ps.quotes_backslash_state, ct.plain_type ), ps.quotes_state ),
      /* */
      ((ps.apostrophe_backslash_state, ct.undefined_type ), ps.undefined_state ),
      ((ps.apostrophe_backslash_state, ct.quotes_type ), ps.apostrophe_state ),
      ((ps.apostrophe_backslash_state, ct.apostrophe_type ), ps.apostrophe_state ),
      ((ps.apostrophe_backslash_state, ct.backslash_type ), ps.apostrophe_state ),
      ((ps.apostrophe_backslash_state, ct.plain_type ), ps.apostrophe_state ),
      /* */
      ((ps.plain, ct.undefined_type ), ps.undefined_state ),
      ((ps.plain, ct.quotes_type ), ps.quotes_state ),
      ((ps.plain, ct.apostrophe_type ), ps.apostrophe_state ),
      ((ps.plain, ct.backslash_type ), ps.plain ),
      ((ps.plain, ct.plain_type ), ps.plain )
    )

  def next_parser_state (parser_state: ParserState, char_type: CharType ): ParserState =
    transitions_that_change_states.getOrElse ((parser_state, char_type ), parser_state )

}

case class ParserTransition_ ()
  extends
    ParserTransition
{

}
