package soda.translator.replacement

/*
 * This package contains auxiliary classes for string manipulation,
 * especially related to replacement.
 */





/**
 * This models all the possible states that the parser can be.
 */

trait ParserState
  extends
    soda.lib.EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class ParserState_ (ordinal : Int, name : String) extends ParserState

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

  lazy val values = Seq (undefined_state, quotes_state, apostrophe_state, quotes_backslash_state, apostrophe_backslash_state, plain)

  def is_same_class (x : ParserState) (y : ParserState) : Boolean =
     (x == y) || _is_like (x) (y) || _is_like (y) (x)

  private def _is_like (x : ParserState) (y : ParserState) : Boolean =
     (x == quotes_state && y == quotes_backslash_state) ||
       (x == apostrophe_state && y == apostrophe_backslash_state)

}

case class ParserStateEnum_ () extends ParserStateEnum

trait ParserTransition
{

  lazy val ps = ParserStateEnum_ ()

  lazy val ct = CharTypeEnum_ ()

  lazy val transitions_that_change_states : Map [ Tuple2 [ParserState, CharType], ParserState] =
    Map (
      /* */
      Tuple2 ( Tuple2 (ps.quotes_state, ct.undefined_type), ps.undefined_state),
      Tuple2 ( Tuple2 (ps.quotes_state, ct.quotes_type), ps.plain),
      Tuple2 ( Tuple2 (ps.quotes_state, ct.backslash_type), ps.quotes_backslash_state),
      /* */
      Tuple2 ( Tuple2 (ps.apostrophe_state, ct.undefined_type), ps.undefined_state),
      Tuple2 ( Tuple2 (ps.apostrophe_state, ct.apostrophe_type), ps.plain),
      Tuple2 ( Tuple2 (ps.apostrophe_state, ct.backslash_type), ps.apostrophe_backslash_state),
      /* */
      Tuple2 ( Tuple2 (ps.quotes_backslash_state, ct.undefined_type), ps.undefined_state),
      Tuple2 ( Tuple2 (ps.quotes_backslash_state, ct.quotes_type), ps.quotes_state),
      Tuple2 ( Tuple2 (ps.quotes_backslash_state, ct.apostrophe_type), ps.quotes_state),
      Tuple2 ( Tuple2 (ps.quotes_backslash_state, ct.backslash_type), ps.quotes_state),
      Tuple2 ( Tuple2 (ps.quotes_backslash_state, ct.plain_type), ps.quotes_state),
      /* */
      Tuple2 ( Tuple2 (ps.apostrophe_backslash_state, ct.undefined_type), ps.undefined_state),
      Tuple2 ( Tuple2 (ps.apostrophe_backslash_state, ct.quotes_type), ps.apostrophe_state),
      Tuple2 ( Tuple2 (ps.apostrophe_backslash_state, ct.apostrophe_type), ps.apostrophe_state),
      Tuple2 ( Tuple2 (ps.apostrophe_backslash_state, ct.backslash_type), ps.apostrophe_state),
      Tuple2 ( Tuple2 (ps.apostrophe_backslash_state, ct.plain_type), ps.apostrophe_state),
      /* */
      Tuple2 ( Tuple2 (ps.plain, ct.undefined_type), ps.undefined_state),
      Tuple2 ( Tuple2 (ps.plain, ct.quotes_type), ps.quotes_state),
      Tuple2 ( Tuple2 (ps.plain, ct.apostrophe_type), ps.apostrophe_state),
      Tuple2 ( Tuple2 (ps.plain, ct.backslash_type), ps.plain),
      Tuple2 ( Tuple2 (ps.plain, ct.plain_type), ps.plain)
    )

  def next_parser_state (parser_state : ParserState) (char_type : CharType) : ParserState =
    transitions_that_change_states.getOrElse ( Tuple2 (parser_state, char_type), parser_state)

}

case class ParserTransition_ () extends ParserTransition
