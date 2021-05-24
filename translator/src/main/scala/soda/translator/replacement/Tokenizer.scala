package soda.translator.replacement


/**
 * A token is a piece of code, that can contain one or more words combined with symbols.
 */
case class Token (text: String, parser_state: ParserState, index: Int ) {

  override
  lazy val toString: String =
    "(\"" + text + "\"," + parser_state + ", " + index + ")"
}

/**
 * This class processes a line to divide it into tokens.
 */
case class Tokenizer (line: String ) {
  import soda.lib.Rec

  lazy val get_tokens: Seq [Token] =
    _postproc (Rec () .foldLeft (Rec () .range (line.length ), _initial_value, _next_value )  )

  case class FoldTuple (last_index: Int, parser_state: ParserState, rev_tokens: Seq [Token]  )

  lazy val _initial_value = FoldTuple (0, ParserStateEnum () .Plain, Seq ()  )

  def _postproc (tuple: FoldTuple ): Seq [Token] =
    (tuple.rev_tokens.+: (Token (line.substring (tuple.last_index ), tuple.parser_state, tuple.last_index )  )  )
      .reverse

  def _next_value (tuple: FoldTuple, current_index: Int ): FoldTuple = {
    lazy val ch = line.charAt (current_index )
    lazy val char_type = CharTypeEnum () .get_char_type (ch )
    lazy val new_parser_state = ParserTransition () .next_parser_state (tuple.parser_state, char_type )

    if (ParserStateEnum () .is_same_class (new_parser_state, tuple.parser_state )
    ) FoldTuple (tuple.last_index, new_parser_state, tuple.rev_tokens )
    else _next_value_of_different_class (tuple, current_index, new_parser_state )
  }

  def _next_value_of_different_class (tuple: FoldTuple, current_index: Int, new_parser_state: ParserState ): FoldTuple = {
    lazy val index =
      if (tuple.parser_state == ParserStateEnum () .QuotesState ||
         tuple.parser_state == ParserStateEnum () .ApostropheState
      ) current_index + 1
      else current_index

    lazy val text = line.substring (tuple.last_index, index )
    FoldTuple (index, new_parser_state, tuple.rev_tokens.+: (Token (text, tuple.parser_state, tuple.last_index )  )  )
  }
}
