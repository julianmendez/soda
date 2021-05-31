package soda.translator.language


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
 *
 * 'def' is for function definition.
 * If it does not fit in any of the 'val' cases.
 *
 * Formerly there was another case for 'val'.
 * Deprecated Case: The first non-blank character of a line is an open parenthesis, e.g. `(x, y) = (0, 1)`
 * This was implemented simply as:
 * `line.trim.startsWith(SodaOpeningParenthesis)`
 * This is no longer supported.
 *
 * @param line line
 * @return a translated line
 */
case class DefinitionTranslator (line: String ) {
  import soda.lib.OptionSD
  import soda.lib.SomeSD
  import soda.translator.replacement.Replacement

  lazy val SodaOpeningParenthesis: String = "("
  lazy val SodaSpace: String = " "
  lazy val ScalaSpace: String = " "

  lazy val get_translation = find_definition (line ) .opt (ifEmpty = line, ifNonEmpty = position => try_found_definition (position ) .line  )

  lazy val is_class_definition =
    indexOf (line, SodaSpace + Translation () .SodaClassReservedWord + SodaSpace ) .isDefined

  lazy val translate_class_definition =
    Replacement (line ) .replace_all (SodaSpace + Translation () .SodaDefinition, "")

  lazy val translate_val_definition =
    Replacement (line ) .add_after_spaces (Translation () .ScalaValue + ScalaSpace )

  lazy val translate_def_definition =
    Replacement (line ) .add_after_spaces (Translation () .ScalaDefinition + ScalaSpace )

  def try_found_definition (position: Int ): Replacement =
    if (is_class_definition ) translate_class_definition
    else if (is_val_definition (position ) ) translate_val_definition
    else translate_def_definition

  def is_val_definition (initial_position: Int ) =
    {
      lazy val position_of_first_opening_parenthesis = indexOf (line, SodaOpeningParenthesis )
      lazy val case1 = position_of_first_opening_parenthesis.isEmpty
      lazy val case2 = position_of_first_opening_parenthesis.opt (false, position => position > initial_position )
      lazy val case3 =
        indexOf (line, Translation () .SodaColon ) .opt (ifEmpty = false, ifNonEmpty = other_position =>
            position_of_first_opening_parenthesis.opt (false, position => position > other_position )        )
      case1 || case2 || case3 }

  /**
   * A line is a definition when its main operator is "=" (the equals sign), which in this context is also called the definition sign.
   * This function finds the first occurrence of the definition sign, if it is present.
   *
   * @param line line
   * @return maybe the position of the definition sign
   */
  def find_definition (line: String ): OptionSD [Int] =
    if (line.endsWith (SodaSpace + Translation () .SodaDefinition )
    ) SomeSD (line.length - Translation () .SodaDefinition.length )
    else indexOf (line, SodaSpace + Translation () .SodaDefinition + SodaSpace )

  def indexOf (line: String, pattern: String ): OptionSD [Int] = indexOf (line, pattern, 0 )

  def indexOf (line: String, pattern: String, start: Int ): OptionSD [Int] =
    SomeSD (line.indexOf (pattern, start )  )
      .filter (position => ! (position == -1 )  )
}
