package soda.translator.parser.tool

/*
 * This package contains tools to parse the Soda language.
 */





trait CommentDelimiterRemover
{



  import   soda.translator.parser.SodaConstant

  private lazy val _sc = SodaConstant .mk

  private lazy val _empty_line = ""

  private lazy val _comment_opening_prefix = _sc .comment_opening_symbol + _sc .space

  private lazy val _documentation_comment_opening_prefix =
    _sc .documentation_comment_opening_symbol + _sc .space

  private lazy val _comment_line_prefix = _sc .comment_line_symbol + _sc .space

  private lazy val _comment_closing_suffix = _sc .space + _sc .comment_closing_symbol

  private def _is_single_delimiter (line : String) : Boolean =
    (line == _documentation_comment_opening_prefix .trim) ||
    (line == _comment_opening_prefix .trim) ||
    (line == _comment_line_prefix .trim) ||
    (line == _comment_closing_suffix .trim)

  private def _remove_single_delimiters (line : String) : String =
    if ( _is_single_delimiter (line)
    ) _empty_line
    else line

  private def _remove_prefix (prefix : String) (line : String) : String =
    if ( line .startsWith (prefix)
    ) line .substring (prefix .length)
    else line

  private def _remove_prefixes (line : String) : String =
    _remove_prefix (_comment_opening_prefix) (
      _remove_prefix (_documentation_comment_opening_prefix) (
        _remove_prefix (_comment_line_prefix) (line)
      )
    )

  private def _remove_suffix (suffix : String) (line : String) : String =
    if ( line .endsWith (suffix)
    ) line .substring (0 , line .length - suffix .length)
    else line

  private def _remove_suffixes (line : String) : String =
    _remove_suffix (_comment_closing_suffix) (line)

  def remove_comment_delimiters (lines : Seq [String] ) : Seq [String] =
    lines .map ( line =>
      _remove_single_delimiters (
        _remove_suffixes (
          _remove_prefixes (line .trim)
        )
      )
    )

}

case class CommentDelimiterRemover_ () extends CommentDelimiterRemover

object CommentDelimiterRemover {
  def mk : CommentDelimiterRemover =
    CommentDelimiterRemover_ ()
}


/**
 * A line containing the definition sign will be classified as a definition.
 * The definitions need to be identified as 'val' case, 'def' case, or 'def' reserved word.
 *
 * When the 'def' reserved word is not detected at the beginning of the line, the following cases need to be determined.
 *
 * 'val' is for value definition.
 * It is detected in three cases.
 * Case 1: The line does not have a opening parenthesis, e.g. `a = 1`
 * Case 2: The first opening parenthesis is after the definition sign, e.g. `x = f (y)`
 * Case 3: The first opening parenthesis or bracket is after a colon,
 *   e.g. `x : (A, B) -> C = (x, y) -> f (x, y)`
 * Case 4: The first non-blank character of a line is an opening parenthesis,
 *   e.g. `(x, y) = (0, 1)`
 *
 * 'def' is for function definition.
 * If it does not fit in any of the 'val' cases.
 *
 * Formerly there was another case for 'val'.
 * Deprecated Case:
 * This was implemented simply as:
 * `line.trim.startsWith (soda_opening_parenthesis)`
 * This is no longer supported.
 *
 */

trait FunctionDefinitionLineDetector
{

  def   line : String

  import   soda.lib.OptionSD
  import   soda.lib.SomeSD
  import   soda.lib.SomeSD_
  import   soda.translator.parser.SodaConstant

  private lazy val _sc = SodaConstant .mk

  private lazy val _trimmed_line : String = line.trim

  private lazy val _fc = FunctionDefinitionTypeEnum .mk

  private def _get_index_from (line : String) (pattern : String) (start : Int) : OptionSD [Int] =
    SomeSD .mk (line .indexOf (pattern, start) )
       .filter ( position => ! (position == -1) )

  private def _get_index (line : String) (pattern : String) : OptionSD [Int] =
    _get_index_from (line) (pattern) (0)

  private lazy val _position_of_first_opening_parenthesis : OptionSD [Int] =
    _get_index (line) (_sc .opening_parenthesis_symbol)

  private lazy val _position_of_first_opening_bracket : OptionSD [Int] =
    _get_index (line) (_sc .opening_bracket_symbol)

  private def _min (a : Int) (b : Int) : Int =
    if ( a < b
    ) a
    else b

  private def _position_of_first_opening_parenthesis_or_bracket_with (index1 : Int) : Int =
    _position_of_first_opening_bracket match  {
      case SomeSD_ (index2) => _min (index1) (index2)
      case _otherwise => index1
    }

  private lazy val _position_of_first_opening_parenthesis_or_bracket : OptionSD [Int] =
    _position_of_first_opening_parenthesis match  {
      case SomeSD_ (index1) =>
        SomeSD .mk (_position_of_first_opening_parenthesis_or_bracket_with (index1) )
      case _otherwise => _position_of_first_opening_bracket
    }

  private lazy val _is_val_definition_case_1 : Boolean =
    _position_of_first_opening_parenthesis .isEmpty

  private def _is_val_definition_case_2 (initial_position : Int) : Boolean =
    _position_of_first_opening_parenthesis match  {
      case SomeSD_ (position) => (position > initial_position)
      case _otherwise => false
    }

  private def _is_val_definition_case_2_b (initial_position : Int) : Boolean =
    _position_of_first_opening_parenthesis_or_bracket match  {
      case SomeSD_ (position) => (position > initial_position)
      case _otherwise => false
    }

  private lazy val _is_val_definition_case_3 : Boolean =
    (_get_index (line) (_sc .type_membership_symbol) ) match  {
      case SomeSD_ (other_position) => _is_val_definition_case_2_b (other_position)
      case _otherwise => false
    }

  private lazy val _is_val_definition_case_4 : Boolean =
    _trimmed_line .startsWith (_sc .opening_parenthesis_symbol)

  private def _is_val_definition (initial_position : Int) : Boolean =
    _is_val_definition_case_1 ||
    _is_val_definition_case_2 (initial_position) ||
    _is_val_definition_case_3 ||
    _is_val_definition_case_4

  private def _try_found_definition_without_def_reserved_word (position : Int) : FunctionDefinitionTypeId =
    if ( _is_val_definition (position)
    ) _fc .val_detected
    else _fc .def_detected

  private def _starts_with_def_reserved_word (position : Int) : Boolean =
    line .trim .startsWith (_sc .def_reserved_word + _sc .space)

  private def _try_found_definition (position : Int) : FunctionDefinitionTypeId =
    if ( _starts_with_def_reserved_word (position)
    ) _fc .def_reserved_word_detected
    else _try_found_definition_without_def_reserved_word (position)

  /**
   * A line is a definition when its main operator is "="  (the equals sign),
   * which in this context is also called the definition sign .
   * This function finds the first occurrence of the definition sign, if it is present.
   *
   * @param line line
   * @return maybe the position of the definition sign
   */

  private def _find_definition (line : String) : OptionSD [Int] =
    if ( line .endsWith (_sc .space + _sc .function_definition_symbol)
    ) SomeSD .mk (line .length - _sc .function_definition_symbol .length)
    else _get_index (line) (_sc .space + _sc .function_definition_symbol + _sc .space)

  lazy val detect : FunctionDefinitionTypeId =
    _find_definition (line) match  {
      case SomeSD_ (position) => _try_found_definition (position)
      case _otherwise => _fc .undetected
    }

}

case class FunctionDefinitionLineDetector_ (line : String) extends FunctionDefinitionLineDetector

object FunctionDefinitionLineDetector {
  def mk (line : String) : FunctionDefinitionLineDetector =
    FunctionDefinitionLineDetector_ (line)
}


trait FunctionDefinitionTypeId
  extends
    soda.lib.EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class FunctionDefinitionTypeId_ (ordinal : Int, name : String) extends FunctionDefinitionTypeId

object FunctionDefinitionTypeId {
  def mk (ordinal : Int) (name : String) : FunctionDefinitionTypeId =
    FunctionDefinitionTypeId_ (ordinal, name)
}

trait FunctionDefinitionTypeEnum
  extends
    soda.lib.Enum [FunctionDefinitionTypeId]
{



  lazy val undefined = FunctionDefinitionTypeId .mk (0) ("undefined")

  lazy val undetected = FunctionDefinitionTypeId .mk (1) ("undetected")

  lazy val val_detected = FunctionDefinitionTypeId .mk (2) ("val_detected")

  lazy val def_detected = FunctionDefinitionTypeId .mk (3) ("def_detected")

  lazy val def_reserved_word_detected = FunctionDefinitionTypeId .mk (4) ("def_reserved_word_detected")

  lazy val values =
    Seq (
      undefined,
      undetected,
      val_detected,
      def_detected,
      def_reserved_word_detected
    )

}

case class FunctionDefinitionTypeEnum_ () extends FunctionDefinitionTypeEnum

object FunctionDefinitionTypeEnum {
  def mk : FunctionDefinitionTypeEnum =
    FunctionDefinitionTypeEnum_ ()
}

