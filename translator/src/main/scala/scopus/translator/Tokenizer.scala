package scopus.translator


case class Token(text: String, parser_state: ParserState, index: Int) {

  override
  val toString: String =
    "(\"" + text + "\"," + parser_state + ", " + index + ")"
}

case class Tokenizer() {

  def tokenize(line: String): Seq[Token] =
    _tokenize_rec(line, 0, 0, ParserStateEnum().Plain, Seq())
      .reverse

  def _next_values (new_parser_state: ParserState, line: String,        last_index: Int, current_index: Int,        parser_state: ParserState, rev_tokens: Seq[Token]) =
    if ( ParserStateEnum().is_same_class(new_parser_state, parser_state)
    ) (last_index, current_index + 1, rev_tokens)
    else {
      val index =
        if ( parser_state == ParserStateEnum().QuotesState ||
           parser_state == ParserStateEnum().ApostropheState
        ) current_index + 1
        else current_index

      val text = line.substring(last_index, index)
      (index, index + 1, rev_tokens.+:(Token(text, parser_state, last_index)))
    }

  import scala.annotation.tailrec
  @tailrec final
  def _tokenize_rec(line: String,        last_index: Int, current_index: Int,        parser_state: ParserState, rev_tokens: Seq[Token]): Seq[Token] =
    if ( current_index >= line.length
    ) rev_tokens.+:(Token(line.substring(last_index), parser_state, last_index))
    else {
      val ch = line.charAt(current_index)
      val charType = CharTypeEnum().get_char_type(ch)
      val new_parser_state = ParserTransition().next_parser_state(parser_state, charType)
      val (new_last_index, new_current_index, new_rev_tokens) =
          _next_values (new_parser_state, line, last_index, current_index, parser_state, rev_tokens)
      _tokenize_rec(line, new_last_index, new_current_index, new_parser_state, new_rev_tokens)
    }

}
