package soda.translator.replacement

/**
 * This is to classify characters.
 */

trait CharType
  extends
    soda.lib.EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class CharType_ (ordinal : Int, name : String) extends CharType

/**
 * This is an enumeration for all types of characters.
 */

trait CharTypeEnum
  extends
    soda.lib.Enum [CharType]
{

  lazy val undefined_type = CharType_ (0, "undefined_type")

  lazy val quotes_type = CharType_ (1, "quotes_type")

  lazy val apostrophe_type = CharType_ (2, "apostrophe_type")

  lazy val backslash_type = CharType_ (3, "backslash_type")

  lazy val plain_type = CharType_ (4, "plain_type")

  lazy val values = Seq (undefined_type, quotes_type, apostrophe_type, backslash_type, plain_type)

  lazy val backslash_char = '\\'

  lazy val quotes_char = '\"'

  lazy val apostrophe_char = '\''

  lazy val underscore_char = '_'

  lazy val symbol_chars : Seq [Char] =
    Seq ('!', '#', '$', '%', '&', '(', ')', '*', '+', ',', '-', '.', '/', ':', ';', '<', '=', '>', '?', '@', '[', ']', '^', '`', '{', '|', '}', '~')

  lazy val simple_char_type_map : Map [Char , CharType] =
    Map (
      Tuple2 ( quotes_char , quotes_type ),
      Tuple2 ( apostrophe_char , apostrophe_type ),
      Tuple2 ( backslash_char, backslash_type )
    )

  def get_char_type (ch : Char) : CharType =
    simple_char_type_map
      .getOrElse (ch, _get_char_type_if_plain (ch) )

  private def _get_char_type_if_plain (ch : Char) : CharType =
    if ( _is_plain (ch)
    ) plain_type
    else undefined_type

  private def _is_plain (ch : Char) : Boolean =
    _is_whitespace (ch) || _is_letter_or_digit_or_underscore (ch) || _is_symbol (ch)

  private def _is_whitespace (ch : Char) : Boolean =
    ch.isWhitespace

  private def _is_letter_or_digit_or_underscore (ch : Char) : Boolean =
    ch.isLetterOrDigit || ch == underscore_char

  private def _is_symbol (ch : Char) : Boolean =
    symbol_chars.contains (ch)

}

case class CharTypeEnum_ () extends CharTypeEnum
