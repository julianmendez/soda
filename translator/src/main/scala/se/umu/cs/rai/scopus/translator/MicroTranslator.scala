package se.umu.cs.rai.scopus.translator

import scala.annotation.tailrec


/**
 * This class translates Scopus source code into Scala source code.
 */
case class MicroTranslator() {

  val NewLine = "\n"

  val ScopusOpeningParenthesis: String = "("
  val ScopusClosingParenthesis: String = ")"
  val ScopusSpace: String = " "
  val ScalaSpace: String = " "

  def translate_program(program: String): String =
    translate_lines(program.split(NewLine).toIndexedSeq).mkString(NewLine) + NewLine

  def tokenize(line: String): Seq[Token] =
    Tokenizer().tokenize(line)

  def translate_lines(lines: Seq[String]): Seq[String] =
    CommentPreprocessor()
      .annotate_lines(lines)
      .map(annotated_line =>
        if ( annotated_line.isComment
        ) annotated_line.line
        else {
          val line = annotated_line.line
          val line_with_space = _add_space_to_scopus_line(line)
          val tokenized_line = tokenize(line_with_space)
          val translated_line = _translate_line(tokenized_line)
          val joint_line = _join_tokens(translated_line)
          val final_line = _remove_space_from_scala_line(joint_line)
          final_line
        }
      )

  def _translate_line(tokens: Seq[Token]): Seq[Token] =
    tokens.map(
      token =>
        if ( token.parser_state == ParserState().Plain
        ) {
          val currentLine = token.text
          val newText = Option(currentLine)
            .flatMap(line => replace(line, ScalaNonScopus(), only_beginning=false))
            .flatMap(line => replace_at_beginning(line, token.index, SynonymAtBeginning()))
            .flatMap(line => replace(line, Synonym(), only_beginning=false))
            .flatMap(line => try_definition(line))
            .flatMap(line => replace_at_beginning(line, token.index, get_translation_table_at_beginning(line)))
            .flatMap(line => replace(line, MainTranslation(), only_beginning=false))
            .getOrElse(currentLine)
          Token(newText, token.parser_state, token.index)
        }
        else token
    )

  def _join_tokens(tokens: Seq[Token]): String =
    tokens
      .map(token => token.text)
      .mkString("")

  def get_translation_table_at_beginning(line: String): Translator =
    if ( line.contains(ScopusOpeningParenthesis)
    ) TranslationAtBeginningWithParen()
    else TranslationAtBeginningWithoutParen()

  /**
   * A line containing the definition sign will be classified as a definition.
   * The definitions need to be translated to either 'val' or 'def'.
   *
   * 'val' is for value definition.
   * It is detected in three cases.
   * Case 1: The line does not have a closing parenthesis, e.g. `a = 1`
   * Case 2: The first non-blank character of a line is an open parenthesis, e.g. `(x, y) = (0, 1)`
   * Case 3: The first closing parenthesis is after the definition sign, e.g. `x = f(y)`
   *
   * 'def' is for function definition.
   * If it does not fit in any of the 'val' cases.
   *
   * @param line line
   * @return maybe a translated line
   */
  def try_definition(line: String): Some[String] = {
    val maybe_position = find_definition(line)
    if ( maybe_position.nonEmpty
    ) {
      val position_of_definition_sign = maybe_position.get
      val position_of_first_closing_parenthesis = line.indexOf(ScopusClosingParenthesis)

      val case1 = position_of_first_closing_parenthesis == -1
      val case2 = line.trim.startsWith(ScopusOpeningParenthesis)
      val case3 = position_of_first_closing_parenthesis > position_of_definition_sign

      if ( case1 || case2 || case3
      ) Some(_add_after_spaces(Translation().ScalaValue + ScalaSpace, line))
      else Some(_add_after_spaces(Translation().ScalaDefinition + ScalaSpace, line))
    }
    else Some(line)
  }

  /**
   * A line is a definition when its main operator is "=" (the equals sign), which in this context is also called the definition sign.
   * This function finds the first occurrence of the definition sign, if it is present.
   *
   * @param line line
   * @return maybe the position of the definition sign
   */
  def find_definition(line: String): Option[Int] = {
    val position =
      if ( line.endsWith(ScopusSpace + Translation().ScopusDefinition)
      ) line.length - Translation().ScopusDefinition.length
      else line.indexOf(ScopusSpace + Translation().ScopusDefinition + ScopusSpace)
    if ( position == -1
    ) None
    else Some(position)
  }

  def replace_all(line: String, pattern: String, replacement: String): String =
    _replace_all_rec(line, pattern, replacement, Seq())

  @tailrec final
  def _replace_all_rec(line: String, pattern: String, replacement: String, replaced_text_rev: Seq[String]): String = {
    val pos = line.indexOf(pattern)
    if ( pos == -1
    )
      replaced_text_rev.+:(line)
        .reverse
        .mkString("")
    else {
      val new_replaced_text_rev = replaced_text_rev.+:(line.substring(0, pos) + replacement)
      val new_line = line.substring(pos + pattern.length)
      _replace_all_rec(new_line, pattern, replacement, new_replaced_text_rev)
    }
  }

  def replace_at_beginning(line: String, index: Int, translator: Translator): Some[String] =
    if ( index == 0
    ) replace(line, translator, only_beginning=true)
    else Some(line)


  def replace(line: String, translator: Translator, only_beginning: Boolean): Some[String] =
    Some(_replace_rec(line, translator.keys, translator, only_beginning))

  @tailrec final
  def _replace_rec(line: String, to_replace: Seq[String], translator: Translator, only_beginning: Boolean): String =
    if ( to_replace.isEmpty
    ) line
    else {
      val reserved_word = to_replace.head
      val alreadyProcessedLine =
        replace_if_found(line, ScopusSpace + reserved_word + ScopusSpace, ScalaSpace + translator.translate(reserved_word) + ScalaSpace, only_beginning)
      _replace_rec(alreadyProcessedLine, to_replace.tail, translator, only_beginning)
    }

  def replace_if_found(line: String, pattern: String, newText: String, only_beginning: Boolean): String =
    if ( (only_beginning && line.trim.startsWith(pattern.trim)) ||
      ( ! only_beginning && line.contains(pattern))
    ) replace_all(line, pattern, newText)
    else line

  def _add_space_to_scopus_line(line: String): String = ScopusSpace + line + ScopusSpace

  def _remove_space_from_scala_line(line: String): String = {
    val line_without_starting_space =
      if ( line.startsWith(ScalaSpace)
      ) line.substring(1)
      else line

    val line_without_ending_space =
      if ( line_without_starting_space.endsWith(ScalaSpace)
      ) line_without_starting_space.substring(0, line_without_starting_space.length - 1)
      else line_without_starting_space

    line_without_ending_space
  }

  def _add_after_spaces(text_to_prepend: String, line: String): String = {
    val prefix_length = line.takeWhile(ch => ch.isSpaceChar).length
    line.substring(0, prefix_length) + text_to_prepend + line.substring(prefix_length)
  }

}
