package soda.translator.language


/**
 * This class translates Soda source code into Scala source code.
 */
case class MicroTranslator () {
  import soda.lib.Rec
  import soda.lib.OptionSD
  import soda.lib.NoneSD
  import soda.lib.SomeSD
  import soda.lib.OptionSDBuilder
  import soda.translator.replacement.CommentPreprocessor
  import soda.translator.replacement.ParserStateEnum
  import soda.translator.replacement.Replacement
  import soda.translator.replacement.Token
  import soda.translator.replacement.Tokenizer
  import soda.translator.replacement.Translator

  lazy val NewLine = "\n"
  lazy val Comma = ","

  lazy val SodaOpeningParenthesis: String = "("
  lazy val SodaClosingParenthesis: String = ")"
  lazy val SodaOpeningBracket: String = "["
  lazy val SodaClosingBracket: String = "]"
  lazy val SodaSpace: String = " "
  lazy val ScalaSpace: String = " "

  def translate_program (program: String ): String = {
    lazy val original_lines = split_lines (program )
    lazy val lines_to_translate = join_lines_ending_with_comma_or_opening_parenthesis (original_lines )
    lazy val translated_lines = translate_lines (lines_to_translate )
    join_translated_lines (translated_lines )
  }

  def split_lines (program: String ): Seq [String] =
    program.split (NewLine ) .toIndexedSeq

  def join_lines_ending_with_comma_or_opening_parenthesis (lines: Seq [String]  ): Seq [String] = {
    lazy val result = processed_lines.reverse

    lazy val processed_lines = {
      lazy val pairs = Rec () .foldLeft (lines, initial_value, next_value )
      if (pairs.in_process_rev.isEmpty
      ) pairs.processed_rev
      else pairs.processed_rev.+: (rev_list_as_element (pairs.in_process_rev, "")  )
    }

    case class FoldTuple (in_process_rev: Seq [String], processed_rev: Seq [String]  )

    lazy val initial_value = FoldTuple (Seq (), Seq ()  )

    def next_value (pair: FoldTuple, head: String ): FoldTuple =
      if (head.trim () .endsWith (Comma ) || head.trim () .endsWith (SodaOpeningParenthesis )
      ) FoldTuple (pair.in_process_rev.+: (head ), pair.processed_rev )
      else {
        lazy val new_head = rev_list_as_element (pair.in_process_rev, head )
        FoldTuple (Seq (), pair.processed_rev.+: (new_head )  )
      }

    def rev_list_as_element (in_process_rev: Seq [String], line: String ): String =
      in_process_rev.reverse.mkString ("") + line

    result
  }

  def join_translated_lines (lines: Seq [String]  ): String =
    lines.mkString (NewLine ) + NewLine

  def translate_lines (lines: Seq [String]  ): Seq [String] =
    CommentPreprocessor (lines )
      .get_annotated_lines
      .map (annotated_line =>
        if (annotated_line.isComment
        ) annotated_line.line
        else _translate_non_comment (annotated_line.line )
      )

  def _translate_non_comment (line: String ): String = {
    lazy val line_with_space = Replacement (line ) .add_space_to_soda_line () .line
    lazy val tokenized_line = Tokenizer (line_with_space ) .get_tokens
    lazy val translated_line = _translate_line (tokenized_line )
    lazy val joint_line = _join_tokens (translated_line )
    lazy val final_line = Replacement (joint_line ) .remove_space_from_scala_line () .line
    final_line
  }

  def _translate_line (tokens: Seq [Token]  ): Seq [Token] =
    tokens.map (token =>
        if (token.parser_state == ParserStateEnum () .Plain
        ) {
          lazy val tr = Tr ()
          lazy val newText = Replacement (token.text )
            .add_spaces_to_symbols (symbols = Translation () .SodaBracketsAndComma.toSet )
            .replace (tr.ScalaNonSoda ()  )
            .replace_at_beginning (token.index, tr.SynonymAtBeginning ()  )
            .replace (tr.Synonym ()  )
            .replace_with (try_definition )
            .replace_at_beginning (token.index, get_translation_table_at_beginning (token.text )  )
            .replace (tr.MainTranslation ()  )
            .replace_regex (tr.Beautifier ()  )
            .line
          Token (newText, token.parser_state, token.index )
        }
        else token
    )

  def get_translation_table_at_beginning (line: String ): Translator =
    if (line.contains (SodaOpeningParenthesis )
    ) Tr () .TranslationAtBeginningWithParen ()
    else Tr () .TranslationAtBeginningWithoutParen ()

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
  def try_definition (line: String ): String = {

    lazy val result = find_definition (line ) .opt (ifEmpty = line, ifNonEmpty = position => try_found_definition (position ) .line
    )

    def try_found_definition (position: Int ): Replacement =
      if (is_class_definition ) translate_class_definition
      else if (is_val_definition (position ) ) translate_val_definition
      else translate_def_definition

    lazy val is_class_definition =
      indexOf (line, SodaSpace + Translation () .SodaClassReservedWord + SodaSpace ) .isDefined

    def is_val_definition (initial_position: Int ) = {
      lazy val position_of_first_opening_parenthesis = indexOf (line, SodaOpeningParenthesis )

      lazy val case1 = position_of_first_opening_parenthesis.isEmpty
      lazy val case2 = position_of_first_opening_parenthesis.opt (false, position => position > initial_position )
      lazy val case3 =
        indexOf (line, Translation () .SodaColon ) .opt (ifEmpty = false, ifNonEmpty = other_position =>
            position_of_first_opening_parenthesis.opt (false, position => position > other_position )
        )

      case1 || case2 || case3
    }

    lazy val translate_class_definition =
      Replacement (line ) .replace_all (SodaSpace + Translation () .SodaDefinition, "")

    lazy val translate_val_definition =
      Replacement (line ) .add_after_spaces (Translation () .ScalaValue + ScalaSpace )

    lazy val translate_def_definition =
      Replacement (line ) .add_after_spaces (Translation () .ScalaDefinition + ScalaSpace )

    result
  }


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

  def _join_tokens (tokens: Seq [Token]  ): String =
    tokens
      .map (token => token.text )
      .mkString ("")

  case class Excerpt (beginning: Int, end: Int )

}
