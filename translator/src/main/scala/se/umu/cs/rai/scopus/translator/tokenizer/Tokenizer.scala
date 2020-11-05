package se.umu.cs.rai.scopus.translator.tokenizer

import scala.annotation.tailrec

case class Token(program: String, parserState: Int, startPos: Int, endPos: Int) {

  override def toString: String =
    "(\"" + text + "\"," + parserState + "," + startPos + "," + endPos + ")"

  def text: String = program.substring(startPos, endPos)

}

case class Tokenizer() {

  def tokenize(program: String): Seq[Token] =
    tokenizeRec(program, 0, 0, ParserState().PlainWhitespaceState, Seq())
      .reverse

  @tailrec
  final def tokenizeRec(program: String, lastIndex: Int, currentIndex: Int, parserState: Int, revTokens: Seq[Token]): Seq[Token] = {
    if (currentIndex >= program.length) {
      Token(program = program, parserState = parserState, startPos = lastIndex, endPos = currentIndex) +: revTokens
    } else {
      val ch = program.charAt(currentIndex)
      val charType = CharType().charType(ch)
      val newParserState = ParserTransition().nextParserState(parserState = parserState, charType = charType)
      val (newLastIndex, newCurrentIndex, newRevTokens) =
        if (newParserState == parserState && parserState != ParserState().PlainWhitespaceState) {
          (lastIndex, currentIndex + 1, revTokens)
        }
        else {
          val index =
            if (parserState == ParserState().QuotesState || parserState == ParserState().ApostropheState) {
              currentIndex + 1
            } else {
              currentIndex
            }
          (index, index + 1, Token(program = program, parserState = parserState, startPos = lastIndex, endPos = index) +: revTokens)
        }
      tokenizeRec(program, newLastIndex, newCurrentIndex, newParserState, newRevTokens)
    }
  }

}
