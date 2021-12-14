package soda.translator.replacement

/**
 * This is to classify characters.
 */
trait CharType  extends soda.lib.EnumConstant

case class CharType_ (ordinal: Int, name: String )  extends CharType

/**
 * This is an enumeration for all types of characters.
 */
trait CharTypeEnum  extends soda.lib.Enum [CharType] {

  lazy val undefined_type = CharType_ (0, "undefined_type")

  lazy val quotes_type = CharType_ (1, "quotes_type")

  lazy val apostrophe_type = CharType_ (2, "apostrophe_type")

  lazy val backslash_type = CharType_ (3, "backslash_type")

  lazy val plain_type = CharType_ (4, "plain_type")

  lazy val values = Seq (undefined_type, quotes_type, apostrophe_type, backslash_type, plain_type )

  lazy val backslash_char = '\\'

  lazy val quotes_char = '\"'

  lazy val apostrophe_char = '\''

  lazy val underscore_char = '_'

  lazy val symbol_chars = Seq ('!', '#', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '`', '{', '|', '}', '~')

  def get_char_type (ch: Char ): CharType =
    if (is_quotes (ch ) ) quotes_type
    else if (is_apostrophe (ch ) ) apostrophe_type
    else if (is_backslash (ch ) ) backslash_type
    else if (is_whitespace (ch ) || is_letter_or_digit_or_underscore (ch ) || is_symbol (ch ) ) plain_type
    else undefined_type

  def is_whitespace (ch: Char ): Boolean =
    ch.isWhitespace

  def is_letter_or_digit_or_underscore (ch: Char ): Boolean =
    ch.isLetterOrDigit || ch == underscore_char

  def is_symbol (ch: Char ): Boolean =
    symbol_chars.contains (ch )

  def is_quotes (ch: Char ): Boolean =
    ch == quotes_char

  def is_apostrophe (ch: Char ): Boolean =
    ch == apostrophe_char

  def is_backslash (ch: Char ): Boolean =
    ch == backslash_char

}

case class CharTypeEnum_ ()  extends CharTypeEnum
