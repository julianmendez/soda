package soda.translator.extension.toscala

trait LineTranslator {

  def line: String

}

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
 * Case 2: The first opening parenthesis is after the definition sign, e.g. `x = f(y)`
 * Case 3: The first opening parenthesis is after a colon, e.g. `x: (A, B) -> C = (x, y) -> f(x,y)`
 * Case 4: The first non-blank character of a line is an open parenthesis, e.g. `(x, y) = (0, 1)`
 *
 * 'def' is for function definition.
 * If it does not fit in any of the 'val' cases.
 *
 * Formerly there was another case for 'val'.
 * Deprecated Case:
 * This was implemented simply as:
 * `line.trim.startsWith(soda_opening_parenthesis)`
 * This is no longer supported.
 *
 */
trait DefinitionTranslatorToScala  extends LineTranslator {

  import soda.lib.OptionSD
  import soda.lib.SomeSD_
  import soda.translator.replacement.Replacement
  import soda.translator.replacement.Replacement_

  lazy val tc = TranslationConstantToScala_ ()

  lazy val trimmed_line = line.trim

  lazy val soda_space: String = " "

  lazy val scala_space: String = " "

  lazy val translation =
    find_definition (line ) .opt (ifEmpty = line, ifNonEmpty = position => try_found_definition (position ) .line    )

  lazy val is_class_definition =
    get_index (line, soda_space + tc.soda_class_reserved_word + soda_space ) .isDefined

  lazy val translation_of_class_definition =
    {
      lazy val new_text =
        if (ends_with_equals
        ) tc.scala_3_class_definition
        else ""
      lazy val result =
        if (condition_for_type_alias
        ) Replacement_ (line )
        else Replacement_ (line ) .replace_all (soda_space + tc.soda_definition, new_text )
      result }

  lazy val ends_with_equals =
    trimmed_line.endsWith (tc.soda_definition )

  lazy val ends_with_opening_brace =
    trimmed_line.endsWith (tc.soda_opening_brace )

  lazy val contains_equals =
    trimmed_line.contains (tc.soda_definition )

  lazy val condition_for_type_alias =
    contains_equals && ! (ends_with_equals || ends_with_opening_brace )

  lazy val translation_of_val_definition =
    Replacement_ (line ) .add_after_spaces_or_pattern (tc.soda_let_pattern, tc.scala_value + scala_space )

  lazy val translation_of_def_definition =
    Replacement_ (line ) .add_after_spaces_or_pattern (tc.soda_let_pattern, tc.scala_definition + scala_space )

  def try_found_definition (position: Int ): Replacement =
    if (is_class_definition ) translation_of_class_definition
    else if (is_val_definition (position ) ) translation_of_val_definition
    else translation_of_def_definition

  def is_val_definition (initial_position: Int ) =
    is_val_definition_case_1 ||
    is_val_definition_case_2 (initial_position ) ||
    is_val_definition_case_3 ||
    is_val_definition_case_4

  lazy val position_of_first_opening_parenthesis =
    get_index (line, tc.soda_opening_parenthesis )

  lazy val is_val_definition_case_1 =
    position_of_first_opening_parenthesis.isEmpty

  def is_val_definition_case_2 (initial_position: Int ) =
    position_of_first_opening_parenthesis.opt (false, position => position > initial_position )

  lazy val is_val_definition_case_3 =
    get_index (line, tc.soda_colon ) .opt (ifEmpty = false, ifNonEmpty = other_position =>
        position_of_first_opening_parenthesis.opt (false, position => position > other_position )    )

  lazy val is_val_definition_case_4 =
    trimmed_line.startsWith (tc.soda_opening_parenthesis )

  /**
   * A line is a definition when its main operator is "=" (the equals sign), which in this context is also called the definition sign.
   * This function finds the first occurrence of the definition sign, if it is present.
   *
   * @param line line
   * @return maybe the position of the definition sign
   */
  def find_definition (line: String ): OptionSD [Int] =
    if (line.endsWith (soda_space + tc.soda_definition )
    ) SomeSD_ (line.length - tc.soda_definition.length )
    else get_index (line, soda_space + tc.soda_definition + soda_space )

  def get_index (line: String, pattern: String ): OptionSD [Int] =
    get_index (line, pattern, 0 )

  def get_index (line: String, pattern: String, start: Int ): OptionSD [Int] =
    SomeSD_ (line.indexOf (pattern, start )  )
      .filter (position => ! (position == -1 )  )

}

case class DefinitionTranslatorToScala_ (line: String )  extends DefinitionTranslatorToScala
