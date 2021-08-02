package soda.translator.replacement


/**
 * A token is a piece of code, that can contain one or more words combined with symbols.
 */
case class Token (text: String, parser_state: ParserState, index: Int )

/**
 * This class processes a line to divide it into tokens.
 */
trait Tokenizer {
  import soda.lib.Rec

  def line: String

  lazy val get_tokens: Seq [Token] =
    _postproc (Rec () .foldLeft (Rec () .range (line.length ), _initial_value, _next_value )  )

  lazy val _initial_value = TokenizerFoldTuple (0, ParserStateEnum () .plain, Seq ()  )

  def _postproc (tuple: TokenizerFoldTuple ): Seq [Token] =
    (tuple.rev_tokens.+: (Token (line.substring (tuple.last_index ), tuple.parser_state, tuple.last_index )  )  )
      .reverse

  def _next_value (tuple: TokenizerFoldTuple, current_index: Int ): TokenizerFoldTuple =
    {
      lazy val ch = line.charAt (current_index )
      lazy val char_type = CharTypeEnum () .get_char_type (ch )
      lazy val new_parser_state = ParserTransitionImpl () .next_parser_state (tuple.parser_state, char_type )
      lazy val result =
        if (ParserStateEnum () .is_same_class (new_parser_state, tuple.parser_state )
        ) TokenizerFoldTuple (tuple.last_index, new_parser_state, tuple.rev_tokens )
        else _next_value_of_different_class (tuple, current_index, new_parser_state )
      result }

  def _next_value_of_different_class (tuple: TokenizerFoldTuple, current_index: Int, new_parser_state: ParserState ): TokenizerFoldTuple =
    {
      lazy val index =
        if (tuple.parser_state == ParserStateEnum () .quotes_state ||
           tuple.parser_state == ParserStateEnum () .apostrophe_state
        ) current_index + 1
        else current_index
      lazy val text = line.substring (tuple.last_index, index )

      TokenizerFoldTuple (index, new_parser_state, tuple.rev_tokens.+: (Token (text, tuple.parser_state, tuple.last_index )  )  ) }
}

case class TokenizerImpl (line: String ) extends Tokenizer

case class TokenizerFoldTuple (last_index: Int, parser_state: ParserState, rev_tokens: Seq [Token]  )
