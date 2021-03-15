package scopus.translator


case class Token(text: String, parser_state: ParserState, index: Int) {

  override
  lazy val toString: String =
    "(\"" + text + "\"," + parser_state + ", " + index + ")"
}

case class Tokenizer() {

  def tokenize(line: String): Seq[Token] = {
    lazy val result = rec(line, 0, 0, ParserStateEnum().Plain, Seq()).reverse

    import scala.annotation.tailrec
        @tailrec
    def rec(line: String, last_index: Int, current_index: Int,          parser_state: ParserState, rev_tokens: Seq[Token]): Seq[Token] =
      if ( current_index >= line.length
      ) rev_tokens.+:(Token(line.substring(last_index), parser_state, last_index))
      else {
        lazy val ch = line.charAt(current_index)
        lazy val charType = CharTypeEnum().get_char_type(ch)
        lazy val new_parser_state = ParserTransition().next_parser_state(parser_state, charType)
        lazy val (new_last_index, new_current_index, new_rev_tokens) =
            _next_values (new_parser_state, line, last_index, current_index, parser_state, rev_tokens)
        rec(line, new_last_index, new_current_index, new_parser_state, new_rev_tokens)
      }

    result
  }

  def _next_values (new_parser_state: ParserState, line: String,        last_index: Int, current_index: Int,        parser_state: ParserState, rev_tokens: Seq[Token]) =
    if ( ParserStateEnum().is_same_class(new_parser_state, parser_state)
    ) (last_index, current_index + 1, rev_tokens)
    else {
      lazy val index =
        if ( parser_state == ParserStateEnum().QuotesState ||
           parser_state == ParserStateEnum().ApostropheState
        ) current_index + 1
        else current_index

      lazy val text = line.substring(last_index, index)
      (index, index + 1, rev_tokens.+:(Token(text, parser_state, last_index)))
    }

}
