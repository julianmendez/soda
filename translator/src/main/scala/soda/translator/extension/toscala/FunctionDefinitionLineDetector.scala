package soda.translator.extension.toscala

/*
 * This package contains classes for the translation to Scala.
 */





/**
 * A line containing the definition sign will be classified as a definition.
 * The definitions need to be identified as 'val' or 'def'.
 *
 * 'val' is for value definition.
 * It is detected in three cases.
 * Case 1: The line does not have a opening parenthesis, e.g. `a = 1`
 * Case 2: The first opening parenthesis is after the definition sign, e.g. `x = f (y)`
 * Case 3: The first opening parenthesis is after a colon, e.g. `x : (A, B) -> C = (x, y) -> f (x, y)`
 * Case 4: The first non-blank character of a line is an opening parenthesis, e.g. `(x, y) = (0, 1)`
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

  def   line: String

  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_
  import   soda.translator.parser.SodaConstant_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _trimmed_line = line.trim

  lazy val undetected = 0

  lazy val val_detected = 1

  lazy val def_detected = 2

  lazy val detect : Int =
    _find_definition (line).opt (ifEmpty = undetected) (ifNonEmpty =  position => _try_found_definition (position) )

  private def _try_found_definition (position : Int) : Int =
    if ( _is_val_definition (position)
    ) val_detected
    else def_detected

  private def _is_val_definition (initial_position : Int) =
    _is_val_definition_case_1 ||
    _is_val_definition_case_2 (initial_position) ||
    _is_val_definition_case_3 ||
    _is_val_definition_case_4

  private lazy val _position_of_first_opening_parenthesis =
    _get_index (line) (_sc.opening_parenthesis_symbol)

  private lazy val _is_val_definition_case_1 =
    _position_of_first_opening_parenthesis.isEmpty

  private def _is_val_definition_case_2 (initial_position : Int) =
    _position_of_first_opening_parenthesis.opt (false) (  position => position > initial_position)

  private lazy val _is_val_definition_case_3 =
    (_get_index (line) (_sc.type_membership_symbol) ).opt (ifEmpty = false) (ifNonEmpty =  other_position =>
        _position_of_first_opening_parenthesis.opt (false) (  position => position > other_position)
    )

  private lazy val _is_val_definition_case_4 =
    _trimmed_line.startsWith (_sc.opening_parenthesis_symbol)

  /**
   * A line is a definition when its main operator is "="  (the equals sign), which in this context is also called the definition sign.
   * This function finds the first occurrence of the definition sign, if it is present.
   *
   * @param line line
   * @return maybe the position of the definition sign
   */
  private def _find_definition (line : String) : OptionSD [Int] =
    if ( line.endsWith (_sc.space + _sc.function_definition_symbol)
    ) SomeSD_ (line.length - _sc.function_definition_symbol.length)
    else _get_index (line) (_sc.space + _sc.function_definition_symbol + _sc.space)

  private def _get_index (line : String) (pattern : String) : OptionSD [Int] =
    _get_index_from (line) (pattern) (0)

  private def _get_index_from (line : String) (pattern : String) (start : Int) : OptionSD [Int] =
    SomeSD_ (line.indexOf (pattern, start) )
      .filter (  position => ! (position == -1) )

}

case class FunctionDefinitionLineDetector_ (line: String) extends FunctionDefinitionLineDetector
