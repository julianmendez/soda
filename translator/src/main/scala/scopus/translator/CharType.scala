package scopus.translator


case class CharType(index: Int)


case class CharTypeCons() {

  val UndefinedType = CharType(0)
  val QuotesType = CharType(1)
  val ApostropheType = CharType(2)
  val BackslashType = CharType(3)
  val PlainType = CharType(4)

  val BackslashChar = '\\'
  val QuotesChar = '\"'
  val ApostropheChar = '\''
  val UnderscoreChar = '_'
  val SymbolChars = Seq('!', '#', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '`', '{', '|', '}', '~')

  def get_char_type(ch: Char): CharType =
    if ( is_quotes(ch) ) QuotesType
    else if ( is_apostrophe(ch) ) ApostropheType
    else if ( is_backslash(ch) ) BackslashType
    else if ( is_whitespace(ch) || is_letter_or_digit_or_underscore(ch) || is_symbol(ch) ) PlainType
    else UndefinedType

  def is_whitespace(ch: Char): Boolean =
    ch.isWhitespace

  def is_letter_or_digit_or_underscore(ch: Char): Boolean =
    ch.isLetterOrDigit || ch == UnderscoreChar

  def is_symbol(ch: Char): Boolean =
    SymbolChars.contains(ch)

  def is_quotes(ch: Char): Boolean =
    ch == QuotesChar

  def is_apostrophe(ch: Char): Boolean =
    ch == ApostropheChar

  def is_backslash(ch: Char): Boolean =
    ch == BackslashChar

}
