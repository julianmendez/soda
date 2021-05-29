package soda.translator.language


/**
 * This class translates Soda source code into Scala source code.
 */
case class MicroTranslator () {
  import soda.translator.replacement.CommentPreprocessor
  import soda.translator.replacement.ParserStateEnum
  import soda.translator.replacement.Replacement
  import soda.translator.replacement.Token
  import soda.translator.replacement.Tokenizer
  import soda.translator.replacement.Translator

  lazy val NewLine = "\n"
  lazy val SodaOpeningParenthesis: String = "("

  def translate_program (program: String ): String =
    {
      lazy val original_lines = split_lines (program )
      lazy val lines_to_translate = join_lines_ending_with_comma_or_opening_parenthesis (original_lines )
      lazy val preprocessed_lines = preprocess_letin_commands (lines_to_translate )
      lazy val translated_lines = translate_lines (preprocessed_lines )
      lazy val translated_program = join_translated_lines (translated_lines )
      translated_program  }

  def split_lines (program: String ): Seq [String] =
    program.split (NewLine ) .toIndexedSeq

  def join_lines_ending_with_comma_or_opening_parenthesis (lines: Seq [String]  ): Seq [String] =
    LineJoiner (lines ) .get_joined_lines

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

  def _translate_non_comment (line: String ): String =
    {
      lazy val line_with_space = Replacement (line ) .add_space_to_soda_line () .line
      lazy val tokenized_line = Tokenizer (line_with_space ) .get_tokens
      lazy val translated_line = _translate_line (tokenized_line )
      lazy val joint_line = _join_tokens (translated_line )
      lazy val final_line = Replacement (joint_line ) .remove_space_from_scala_line () .line
      final_line  }

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

  def try_definition (line: String ): String =
    DefinitionTranslator (line ) .get_translation

  def _join_tokens (tokens: Seq [Token]  ): String =
    tokens
      .map (token => token.text )
      .mkString ("")

  def preprocess_letin_commands (lines: Seq [String]  ): Seq [String] =
    lines.map (line =>
      append_if_condition (line, starts_with_in, Translation () .ScalaInTranslation ) )

  def starts_with_in (line: String ): Boolean =
    line.trim () .startsWith (Translation () .SodaInPattern )

  def append_if_condition (line: String, condition: String => Boolean, to_append: String ): String =
    if (condition (line )
    ) line + to_append
    else line

  case class Excerpt (beginning: Int, end: Int )
}
