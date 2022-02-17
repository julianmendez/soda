package soda.translator.extension.toscala

/**
 * A line containing the definition sign will be classified as a definition.
 * The definitions need to be identified as 'val' or 'def'.
 *
 * 'val' is for value definition.
 * It is detected in three cases.
 * Case 1: The line does not have a opening parenthesis, e.g. `a = 1`
 * Case 2: The first opening parenthesis is after the definition sign, e.g. `x = f (y)`
 * Case 3: The first opening parenthesis is after a colon, e.g. `x:  (A, B) -> C =  (x, y) -> f (x,y)`
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

trait FunctionDefinitionLineTranslator
  extends
    soda.translator.block.LineTranslator
{

  def   line: String

  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.replacement.Replacement
  import   soda.translator.replacement.Replacement_

  lazy val sc = SodaConstant_ ()

  lazy val tc = TranslationConstantToScala_ ()

  lazy val trimmed_line = line.trim

  lazy val soda_space : String = " "

  lazy val scala_space : String = " "

  lazy val translation =
    find_definition (line).opt (ifEmpty = line) (ifNonEmpty =  position => try_found_definition (position).line)

  lazy val translation_of_val_definition =
    Replacement_ (line).add_after_spaces_or_pattern (tc.soda_let_pattern) (tc.scala_value + scala_space)

  lazy val translation_of_def_definition =
    Replacement_ (line).add_after_spaces_or_pattern (tc.soda_let_pattern) (tc.scala_definition + scala_space)

  def try_found_definition (position : Int) : Replacement =
    if ( is_val_definition (position)
    ) translation_of_val_definition
    else translation_of_def_definition

  def is_val_definition (initial_position : Int) =
    is_val_definition_case_1 ||
    is_val_definition_case_2 (initial_position) ||
    is_val_definition_case_3 ||
    is_val_definition_case_4

  lazy val position_of_first_opening_parenthesis =
    get_index (line) (sc.opening_parenthesis_symbol)

  lazy val is_val_definition_case_1 =
    position_of_first_opening_parenthesis.isEmpty

  def is_val_definition_case_2 (initial_position : Int) =
    position_of_first_opening_parenthesis.opt (false) (  position => position > initial_position)

  lazy val is_val_definition_case_3 =
    (get_index (line) (sc.type_membership_symbol) ).opt (ifEmpty = false) (ifNonEmpty =  other_position =>
        position_of_first_opening_parenthesis.opt (false) (  position => position > other_position)
    )

  lazy val is_val_definition_case_4 =
    trimmed_line.startsWith (sc.opening_parenthesis_symbol)

  /**
   * A line is a definition when its main operator is "="  (the equals sign), which in this context is also called the definition sign.
   * This function finds the first occurrence of the definition sign, if it is present.
   *
   * @param line line
   * @return maybe the position of the definition sign
   */
  def find_definition (line : String) : OptionSD [Int] =
    if ( line.endsWith (soda_space + sc.function_definition_symbol)
    ) SomeSD_ (line.length - sc.function_definition_symbol.length)
    else get_index (line) (soda_space + sc.function_definition_symbol + soda_space)

  def get_index (line : String) (pattern : String) : OptionSD [Int] =
    get_index_from (line) (pattern) (0)

  def get_index_from (line : String) (pattern : String) (start : Int) : OptionSD [Int] =
    SomeSD_ (line.indexOf (pattern, start) )
      .filter (  position => ! (position == -1) )

}

case class FunctionDefinitionLineTranslator_ (line: String) extends FunctionDefinitionLineTranslator
