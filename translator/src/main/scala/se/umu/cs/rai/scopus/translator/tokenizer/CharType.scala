package se.umu.cs.rai.scopus.translator.tokenizer


case class CharType() {

  val UndefinedType = 0
  val WhitespaceType = 1
  val QuotesType = 2
  val ApostropheType = 3
  val BackslashType = 4
  val LetterOrDigitOrUnderscoreType = 5
  val SymbolType = 6

  val BackslashChar = '\\'
  val QuotesChar = '\"'
  val ApostropheChar = '\''
  val UnderscoreChar = '_'
  val SymbolChars = Seq('!', '#', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '`', '{', '|', '}', '~')

  def charType(ch: Char): Int = {
    if (isWhitespace(ch)) {
      WhitespaceType
    } else if (isQuotes(ch)) {
      QuotesType
    } else if (isApostrophe(ch)) {
      ApostropheChar
    } else if (isBackslash(ch)) {
      BackslashType
    } else if (isLetterOrDigitOrUnderscore(ch)) {
      LetterOrDigitOrUnderscoreType
    } else if (isSymbol(ch)) {
      SymbolType
    } else {
      UndefinedType
    }
  }

  def isWhitespace(ch: Char): Boolean =
    ch.isWhitespace

  def isLetterOrDigitOrUnderscore(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == UnderscoreChar

  def isSymbol(ch: Char): Boolean =
    SymbolChars.contains(ch)

  def isQuotes(ch: Char): Boolean =
    ch == QuotesChar

  def isApostrophe(ch: Char): Boolean =
    ch == ApostropheChar

  def isBackslash(ch: Char): Boolean =
    ch == BackslashChar

}
