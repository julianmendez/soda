package soda.translator.language

import soda.lib.Rec
import soda.translator.replacement.CommentPreprocessor
import soda.translator.replacement.ParserStateEnum
import soda.translator.replacement.Replacement
import soda.translator.replacement.Token
import soda.translator.replacement.Tokenizer
import soda.translator.replacement.Translator


/**
 * This class translates Soda source code into Scala source code.
 */
case class MicroTranslator () {

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
    lazy val lines_to_translate = join_lines_ending_with_comma (original_lines )
    lazy val translated_lines = translate_lines (lines_to_translate )
    join_translated_lines (translated_lines )
  }

  def split_lines (program: String ): Seq [String] =
    program.split (NewLine ) .toIndexedSeq

  def join_lines_ending_with_comma (lines: Seq [String]  ): Seq [String] = {
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
      if (head.trim () .endsWith (Comma )
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

  def tokenize (line: String ): Seq [Token] =
    Tokenizer () .tokenize (line )

  def translate_lines (lines: Seq [String]  ): Seq [String] =
    CommentPreprocessor ()
      .annotate_lines (lines )
      .map (annotated_line =>
        if (annotated_line.isComment
        ) annotated_line.line
        else _translate_non_comment (annotated_line.line )
      )

  def _translate_non_comment (line: String ): String = {
    lazy val line_with_space = Replacement (line ) .add_space_to_soda_line () .line
    lazy val tokenized_line = tokenize (line_with_space )
    lazy val translated_line = _translate_line (tokenized_line )
    lazy val joint_line = _join_tokens (translated_line )
    lazy val final_line = Replacement (joint_line ) .remove_space_from_scala_line () .line
    final_line
  }

  def _translate_line (tokens: Seq [Token]  ): Seq [Token] =
    tokens.map (
      token =>
        if (token.parser_state == ParserStateEnum () .Plain
        ) {
          lazy val newText = Replacement (token.text )
            .add_spaces_to_symbols (symbols=Translation () .SodaBracketsAndComma.toSet )
            .replace (ScalaNonSoda (), only_beginning=false )
            .replace_at_beginning (token.index, SynonymAtBeginning ()  )
            .replace (Synonym (), only_beginning=false )
            .replace_with (try_definition )
            .replace_with (try_extends_between_square_brackets )
            .replace_at_beginning (token.index, get_translation_table_at_beginning (token.text )  )
            .replace (MainTranslation (), only_beginning=false )
            .replace_regex (Beautifier ()  )
            .line
          Token (newText, token.parser_state, token.index )
        }
        else token
    )

  def _join_tokens (tokens: Seq [Token]  ): String =
    tokens
      .map (token => token.text )
      .mkString ("")

  def get_translation_table_at_beginning (line: String ): Translator =
    if (line.contains (SodaOpeningParenthesis )
    ) TranslationAtBeginningWithParen ()
    else TranslationAtBeginningWithoutParen ()

  /**
   * A line containing the definition sign will be classified as a definition.
   * The definitions need to be translated to either 'val' or 'def'.
   *
   * 'val' is for value definition.
   * It is detected in three cases.
   * Case 1: The line does not have a closing parenthesis, e.g. `a = 1`
   * Case 2: The first closing parenthesis is after the definition sign, e.g. `x = f(y)`
   * Case 3: The first opening parenthesis is after a colon, e.g. `x: (A, B) -> C = (x, y) -> f(x,y)`
   *
   * 'def' is for function definition.
   * If it does not fit in any of the 'val' cases.
   *
   * Formerly there was another case for 'val'.
   * Deprecated Case: The first non-blank character of a line is an open parenthesis, e.g. `(x, y) = (0, 1)`
   * This was implemented simply as:
   *  `line.trim.startsWith(SodaOpeningParenthesis)`
   * This is no longer supported.
   *
   * @param line line
   * @return a translated line
   */
  def try_definition (line: String ): String = {
    lazy val maybe_position_of_definition = find_definition (line )
    if (maybe_position_of_definition.nonEmpty
    ) {
      lazy val position_of_first_closing_parenthesis = line.indexOf (SodaClosingParenthesis )
      lazy val position_of_first_opening_parenthesis = line.indexOf (SodaOpeningParenthesis )

      lazy val case1 = position_of_first_closing_parenthesis == -1
      lazy val case2 = position_of_first_closing_parenthesis > maybe_position_of_definition.get

      lazy val maybe_position_of_colon = find_pattern (line, Translation () .SodaColon )
      lazy val case3 =
        if (maybe_position_of_colon.nonEmpty
        ) position_of_first_opening_parenthesis > maybe_position_of_colon.get
        else false

      lazy val new_text = if (case1 || case2 || case3
      ) Translation () .ScalaValue + ScalaSpace
      else Translation () .ScalaDefinition + ScalaSpace
      Replacement (line ) .add_after_spaces (new_text ) .line
    }
    else line
  }

  /**
   * A line is a definition when its main operator is "=" (the equals sign), which in this context is also called the definition sign.
   * This function finds the first occurrence of the definition sign, if it is present.
   *
   * @param line line
   * @return maybe the position of the definition sign
   */
  def find_definition (line: String ): Option [Int] =
    if (line.endsWith (SodaSpace + Translation () .SodaDefinition )
    ) Some (line.length - Translation () .SodaDefinition.length )
    else find_pattern (line, SodaSpace + Translation () .SodaDefinition + SodaSpace )

  def find_pattern (line: String, pattern: String ): Option [Int] = {
    lazy val position = line.indexOf (pattern )
    if (position == -1
    ) None
    else Some (position )
  }

  /**
   * This tries to replace an `extends` by a subtype restriction, to define an upper bound of a parametric type.
   * This only applies to a parameter that is between square brackets.
   * @param line line
   * @return maybe a translated line
   */
  def try_extends_between_square_brackets (line: String ): String = {
    lazy val result = Rec () .foldLeft (find_square_brackets (line ), initial_value, next_value )

    lazy val initial_value = line

    def next_value (line: String, position: Excerpt ): String = {
      lazy val substr = line.substring (position.beginning, position.end )
      lazy val new_substr = Replacement (substr )
        .replace (TranslationBetweenSquareBrackets (), only_beginning=false )
        .line
      if (substr == new_substr
      ) line
      else line.substring (0, position.beginning ) + new_substr + line.substring (position.end )
    }

    result
  }

  def find_square_brackets (line: String ): Seq [Excerpt] = {
    lazy val result =
      Rec ()
        .foldLeftWhile (Rec () .range (line.length ), initial_value, next_value, condition )
        .positions

    lazy val initial_value = FoldTuple (Seq (), 0 )

    def next_value (tuple: FoldTuple, x: Int ): FoldTuple = {
      lazy val maybe_pair = find_square_brackets_from (line, tuple.start )
      if (maybe_pair.isEmpty
      ) FoldTuple (tuple.positions, -1 )
      else {
        lazy val pair = maybe_pair.get
        FoldTuple (tuple.positions.+: (pair ), pair.end + SodaClosingBracket.length )
      }
    }

    def condition (tuple: FoldTuple, x: Int ): Boolean =
      (0 <= tuple.start && tuple.start <= line.length )

    case class FoldTuple (positions: Seq [Excerpt], start: Int )

    result
  }

  def find_square_brackets_from (line: String, start: Int ): Option [Excerpt] =
    if (start < 0 || start >= line.length
    ) None
    else {
      lazy val left = line.indexOf (SodaOpeningBracket, start )
      lazy val right =
        if (left == -1
        ) -1
        else line.indexOf (SodaClosingBracket, left + SodaOpeningBracket.length )
      if (right == -1
      ) None
      else Some (Excerpt (left + SodaOpeningBracket.length, right )  )
    }

  case class Excerpt (beginning: Int, end: Int )

}
