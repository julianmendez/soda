package se.umu.cs.rai.scopus.translator

import scala.annotation.tailrec

case class Token(text: String, parserState: Int, index: Int) {

  override
  val toString: String =
    "(\"" + text + "\"," + parserState + ", " + index + ")"
}

case class Tokenizer() {

  def tokenize(line: String): Seq[Token] =
    tokenizeRec(line, 0, 0, ParserState().Plain, Seq())
      .reverse

  @tailrec final
  def tokenizeRec(line: String, lastIndex: Int, currentIndex: Int, parserState: Int, revTokens: Seq[Token]): Seq[Token] =
    if ( currentIndex >= line.length
    ) revTokens.prepended(Token(line.substring(lastIndex), parserState, lastIndex))
    else {
      val ch = line.charAt(currentIndex)
      val charType = CharType().charType(ch)
      val newParserState = ParserTransition().nextParserState(parserState = parserState, charType = charType)
      val (newLastIndex, newCurrentIndex, newRevTokens) =
        if ( newParserState == parserState
        ) (lastIndex, currentIndex + 1, revTokens)
        else {
          val index =
            if ( parserState == ParserState().QuotesState || parserState == ParserState().ApostropheState
            ) currentIndex + 1
            else currentIndex

          val text = line.substring(lastIndex, index)
          (index, index + 1, revTokens.prepended(Token(text, parserState, lastIndex)))
        }
      tokenizeRec(line, newLastIndex, newCurrentIndex, newParserState, newRevTokens)
    }

}
