package soda.translator.extension.tocoq

/**
 * A line containing the definition sign will be classified as a definition.
 * The definitions need to be identified as 'val', 'def', or 'class'.
 *
 * 'class' is for class definition.
 * It is detected if the 'class' reserved word is also in the same line.
 *
 * 'val' is for value definition.
 * It is detected in three cases.
 * Case 1: The line does not have a opening parenthesis, e.g. `a = 1`
 * Case 2: The first opening parenthesis is after the definition sign, e.g. `x = f (y)`
 * Case 3: The first opening parenthesis is after a colon, e.g. `x: (A, B) -> C = (x, y) -> f (x,y)`
 * Case 4: The first non-blank character of a line is an open parenthesis, e.g. `(x, y) =  (0, 1)`
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

trait DefinitionLineTranslator
  extends
    soda.translator.block.LineTranslator
{

  def   line: String

  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.replacement.Replacement
  import   soda.translator.replacement.Replacement_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToCoq_ ()

  private lazy val _trimmed_line = line.trim

  lazy val translation =
    find_definition (line).opt (ifEmpty = line) (ifNonEmpty =  position => _try_found_definition (position).line)

  private lazy val _is_class_definition =
    get_index (line) (_sc.space + _sc.class_reserved_word + _sc.space).isDefined

  private lazy val _translation_of_class_definition =
    if ( _condition_for_type_alias
    ) Replacement_ (line)
    else Replacement_ (line).replace_all (_sc.space + _sc.function_definition_symbol) ("")

  private lazy val _ends_with_equals = false

  private lazy val _ends_with_opening_brace = false

  private lazy val _contains_equals =
    _trimmed_line.contains (_sc.function_definition_symbol)

  private lazy val _condition_for_type_alias =
    _contains_equals && ! (_ends_with_equals || _ends_with_opening_brace)

  private lazy val _translation_of_val_definition =
    Replacement_ (line).add_after_spaces_or_pattern (_tc.coq_space) (_tc.coq_space)

  private lazy val _translation_of_def_definition =
    Replacement_ (line).add_after_spaces_or_pattern (_tc.coq_space) (_tc.coq_space)

  private def _try_found_definition (position : Int) : Replacement =
    if ( _is_class_definition
    ) _translation_of_class_definition
    else _decide_val_or_def_translation (position)

  private def _decide_val_or_def_translation (position : Int) : Replacement =
    if ( _is_val_definition (position)
    ) _translation_of_val_definition
    else _translation_of_def_definition

  private def _is_val_definition (initial_position : Int) =
    _is_val_definition_case_1 ||
    _is_val_definition_case_2 (initial_position) ||
    _is_val_definition_case_3 ||
    _is_val_definition_case_4

  private lazy val _position_of_first_opening_parenthesis =
    get_index (line) (_sc.opening_parenthesis_symbol)

  private lazy val _is_val_definition_case_1 =
    _position_of_first_opening_parenthesis.isEmpty

  private def _is_val_definition_case_2 (initial_position : Int) =
    _position_of_first_opening_parenthesis.opt (false) (  position => (position > initial_position) )

  private lazy val _is_val_definition_case_3 =
    (get_index (line) (_sc.type_membership_symbol) ).opt (ifEmpty = false) (ifNonEmpty =  other_position =>
      _position_of_first_opening_parenthesis.opt (false) (  position => (position > other_position) )
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

  def find_definition (line : String) : OptionSD [Int] =
    if ( line.endsWith (_sc.space + _sc.function_definition_symbol)
    ) SomeSD_ (line.length - _sc.function_definition_symbol.length)
    else get_index (line) (_sc.space + _sc.function_definition_symbol + _sc.space)

  def get_index (line : String) (pattern : String) : OptionSD [Int] =
    get_index_from (line) (pattern) (0)

  def get_index_from (line : String) (pattern : String) (start : Int) : OptionSD [Int] =
    SomeSD_ (line.indexOf (pattern, start) )
      .filter (  position => !  (position == -1) )

}

case class DefinitionLineTranslator_ (line: String) extends DefinitionLineTranslator
