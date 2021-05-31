package soda.translator.language


/**
 * This class translates Soda source code into Scala source code.
 */
case class MicroTranslator () {
  import soda.lib.SomeSD
  import soda.translator.replacement.CommentPreprocessor
  import soda.translator.replacement.ParserStateEnum
  import soda.translator.replacement.Replacement
  import soda.translator.replacement.Token
  import soda.translator.replacement.Tokenizer
  import soda.translator.replacement.Translator

  lazy val NewLine = "\n"
  lazy val SodaOpeningParenthesis: String = "("
  lazy val Tr = TranslatorImplementation ()


  def translate_program (program: String ): String =
    SomeSD (program )
      .map (x => split_lines (x )  )
      .map (x => join_lines_with_opening_brackets (x )  )
      .map (x => preprocess_let_in_commands (x )  )
      .map (x => join_lines_with_closing_brackets (x )  )
      .map (x => translate_lines (x )  )
      .map (x => join_translated_lines (x )  )
      .get

  def split_lines (program: String ): Seq [String] =
    program.split (NewLine ) .toIndexedSeq

  def join_lines_with_opening_brackets (lines: Seq [String]  ): Seq [String] =
    LineJoiner (lines ) .get_joined_lines_with_opening_brackets

  def join_lines_with_closing_brackets (lines: Seq [String]  ): Seq [String] =
    LineJoiner (lines ) .get_joined_lines_with_closing_brackets

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
      SomeSD (line )
        .map (x => Replacement (x ) .add_space_to_soda_line () .line )
        .map (x => Tokenizer (x ) .get_tokens )
        .map (x => _translate_line (x )  )
        .map (x => _join_tokens (x )  )
        .map (x => Replacement (x ) .remove_space_from_scala_line () .line )
        .get

  def _translate_line (tokens: Seq [Token]  ): Seq [Token] =
    tokens.map (token =>
        if (token.parser_state == ParserStateEnum () .Plain
        ) Token (_get_all_replacements (token ), token.parser_state, token.index )
        else token
    )

  def _get_all_replacements (token: Token ): String =
    Replacement (token.text )
      .add_spaces_to_symbols (symbols = Translation () .SodaBracketsAndComma.toSet )
      .replace (Tr.ScalaNonSoda ()  )
      .replace_at_beginning (token.index, Tr.SynonymAtBeginning ()  )
      .replace (Tr.Synonym ()  )
      .replace_with (try_definition )
      .replace_at_beginning (token.index, get_translation_table_at_beginning (token.text )  )
      .replace (Tr.MainTranslation ()  )
      .replace_regex (Tr.Beautifier ()  )
      .line

  def get_translation_table_at_beginning (line: String ): Translator =
    if (line.contains (SodaOpeningParenthesis )
    ) Tr.TranslationAtBeginningWithParen ()
    else Tr.TranslationAtBeginningWithoutParen ()

  def try_definition (line: String ): String =
    DefinitionTranslator (line ) .get_translation

  def _join_tokens (tokens: Seq [Token]  ): String =
    tokens
      .map (token => token.text )
      .mkString ("")

  def preprocess_let_in_commands (lines: Seq [String]  ): Seq [String] =
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
