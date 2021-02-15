package se.umu.cs.rai.scopus.translator

import scala.annotation.tailrec

case class Token(text: String, parserState: Int, index: Int) {

  override
  val toString: String =
    "(\"" + text + "\"," + parserState + ", " + index + ")"
}

case class Tokenizer() {

  def tokenize(line: String): Seq[Token] =
    tokenize_rec(line, 0, 0, ParserState().Plain, Seq())
      .reverse

  def _next_values (newParserState: Int, line: String, lastIndex: Int, currentIndex: Int, parserState: Int, revTokens: Seq[Token]) =
    if ( newParserState == parserState
    ) (lastIndex, currentIndex + 1, revTokens)
    else {
      val index =
        if ( parserState == ParserState().QuotesState || parserState == ParserState().ApostropheState
        ) currentIndex + 1
        else currentIndex

      val text = line.substring(lastIndex, index)
      (index, index + 1, revTokens.+:(Token(text, parserState, lastIndex)))
    }

  @tailrec final
  def tokenize_rec(line: String, lastIndex: Int, currentIndex: Int, parserState: Int, revTokens: Seq[Token]): Seq[Token] =
    if ( currentIndex >= line.length
    ) revTokens.+:(Token(line.substring(lastIndex), parserState, lastIndex))
    else {
      val ch = line.charAt(currentIndex)
      val charType = CharType().get_char_type(ch)
      val newParserState = ParserTransition().next_parser_state(parserState, charType)
      val (newLastIndex, newCurrentIndex, newRevTokens) = _next_values (newParserState, line, lastIndex, currentIndex, parserState, revTokens)
      tokenize_rec(line, newLastIndex, newCurrentIndex, newParserState, newRevTokens)
    }

}
