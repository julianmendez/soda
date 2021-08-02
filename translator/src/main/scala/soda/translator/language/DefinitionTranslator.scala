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
 * `line.trim.startsWith(soda_opening_parenthesis)`
 * This is no longer supported.
 *
 */
trait DefinitionTranslator {
  import soda.lib.OptionSD
  import soda.lib.SomeElem
  import soda.translator.replacement.Replacement
  import soda.translator.replacement.ReplacementImpl

  def line: String

  lazy val soda_opening_parenthesis: String = "("

  lazy val soda_space: String = " "

  lazy val scala_space: String = " "

  lazy val translation =
    find_definition (line ) .opt (ifEmpty = line, ifNonEmpty = position => try_found_definition (position ) .line    )

  lazy val is_class_definition =
    indexOf (line, soda_space + Translation () .soda_class_reserved_word + soda_space ) .isDefined

  lazy val translate_class_definition =
    ReplacementImpl (line ) .replace_all (soda_space + Translation () .soda_definition, "")

  lazy val translate_val_definition =
    ReplacementImpl (line ) .add_after_spaces (Translation () .scala_value + scala_space )

  lazy val translate_def_definition =
    ReplacementImpl (line ) .add_after_spaces (Translation () .scala_definition + scala_space )

  def try_found_definition (position: Int ): Replacement =
    if (is_class_definition ) translate_class_definition
    else if (is_val_definition (position ) ) translate_val_definition
    else translate_def_definition

  def is_val_definition (initial_position: Int ): Boolean =
    {
      lazy val position_of_first_opening_parenthesis = indexOf (line, soda_opening_parenthesis )
      lazy val case1 = position_of_first_opening_parenthesis.isEmpty
      lazy val case2 = position_of_first_opening_parenthesis.opt (false, position => position > initial_position )
      lazy val case3 =
        indexOf (line, Translation () .soda_colon ) .opt (ifEmpty = false, ifNonEmpty = other_position =>
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
    if (line.endsWith (soda_space + Translation () .soda_definition )
    ) SomeElem (line.length - Translation () .soda_definition.length )
    else indexOf (line, soda_space + Translation () .soda_definition + soda_space )

  def indexOf (line: String, pattern: String ): OptionSD [Int] =
    indexOf (line, pattern, 0 )

  def indexOf (line: String, pattern: String, start: Int ): OptionSD [Int] =
    SomeElem (line.indexOf (pattern, start )  )
      .filter (position => ! (position == -1 )  )
}

case class DefinitionTranslatorImpl (line: String ) extends DefinitionTranslator
