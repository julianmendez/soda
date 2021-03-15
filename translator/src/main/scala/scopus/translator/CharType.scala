package scopus.translator


case class CharType(ordinal0: Int, name0: String) extends EnumConstant {
  lazy val ordinal = ordinal0
  lazy val name = name0
}


case class CharTypeEnum() {

  lazy val UndefinedType = CharType(0, "UndefinedType")
  lazy val QuotesType = CharType(1, "QuotesType")
  lazy val ApostropheType = CharType(2, "ApostropheType")
  lazy val BackslashType = CharType(3, "BackslashType")
  lazy val PlainType = CharType(4, "PlainType")

  lazy val values = Seq(UndefinedType, QuotesType, ApostropheType, BackslashType, PlainType)


  lazy val BackslashChar = '\\'
  lazy val QuotesChar = '\"'
  lazy val ApostropheChar = '\''
  lazy val UnderscoreChar = '_'
  lazy val SymbolChars = Seq('!', '#', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '`', '{', '|', '}', '~')

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
