package se.umu.cs.rai.scopus.translator

case class CharType() {

  val UndefinedType = 0
  val QuotesType = 1
  val ApostropheType = 2
  val BackslashType = 3
  val PlainType = 4

  val BackslashChar = '\\'
  val QuotesChar = '\"'
  val ApostropheChar = '\''
  val UnderscoreChar = '_'
  val SymbolChars = Seq('!', '#', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '`', '{', '|', '}', '~')

  def charType(ch: Char): Int =
    if ( isQuotes(ch) )
      QuotesType
    else if ( isApostrophe(ch) )
      ApostropheChar
    else if ( isBackslash(ch) )
      BackslashType
    else if ( isWhitespace(ch) || isLetterOrDigitOrUnderscore(ch) || isSymbol(ch) )
      PlainType
    else
      UndefinedType

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
