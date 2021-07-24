package soda.translator.language


/**
 * This class translates Soda source code into Scala source code.
 */
trait MicroTranslator {
  import soda.lib.SomeElem
  import soda.translator.replacement.CommentPreprocessorImpl
  import soda.translator.replacement.ParserStateEnumImpl
  import soda.translator.replacement.ReplacementImpl
  import soda.translator.replacement.Token
  import soda.translator.replacement.TokenizerImpl
  import soda.translator.replacement.Translator

  lazy val NewLine = "\n"
  lazy val SodaOpeningParenthesis: String = "("

  lazy val SynonymAtBeginning = DefaultTranslatorImpl (Translation () .SynonymAtBeginning )
  lazy val TranslationAtBeginningWithParen = DefaultTranslatorImpl (Translation () .TranslationAtBeginningWithParen )
  lazy val TranslationAtBeginningWithoutParen = DefaultTranslatorImpl (Translation () .TranslationAtBeginningWithoutParen )
  lazy val Synonym = DefaultTranslatorImpl (Translation () .Synonym )
  lazy val MainTranslation = DefaultTranslatorImpl (Translation () .MainTranslation )
  lazy val ScalaNonSoda = DefaultTranslatorImpl (Translation () .ScalaNonSoda )
  lazy val Beautifier = DefaultTranslatorImpl (Translation () .Beautifier )

  def translate_program (program: String ): String =
    SomeElem (program )
      .map (split_lines )
      .map (join_lines_with_forward_join )
      .map (preprocess_let_in_commands )
      .map (join_lines_with_backward_join )
      .map (translate_lines )
      .map (join_translated_lines )
      .get

  def split_lines (program: String ): Seq [String] =
    program.split (NewLine ) .toIndexedSeq

  def join_lines_with_forward_join (lines: Seq [String]  ): Seq [String] =
    LineJoinerImpl (lines ) .get_joined_lines_with_forward_join

  def join_lines_with_backward_join (lines: Seq [String]  ): Seq [String] =
    LineJoinerImpl (lines ) .get_joined_lines_with_backward_join

  def join_translated_lines (lines: Seq [String]  ): String =
    lines.mkString (NewLine ) + NewLine

  def translate_lines (lines: Seq [String]  ): Seq [String] =
    CommentPreprocessorImpl (lines )
      .get_annotated_lines
      .map (annotated_line =>
        if (annotated_line.isComment
        ) annotated_line.line
        else _translate_non_comment (annotated_line.line )      )

  def _translate_non_comment (line: String ): String =
      SomeElem (line )
        .map (x => ReplacementImpl (x ) .add_space_to_soda_line () .line )
        .map (x => TokenizerImpl (x ) .get_tokens )
        .map (x => _translate_line (x )  )
        .map (x => _join_tokens (x )  )
        .map (x => ReplacementImpl (x ) .remove_space_from_scala_line () .line )
        .get

  def _translate_line (tokens: Seq [Token]  ): Seq [Token] =
    tokens.map (token =>
        if (token.parser_state == ParserStateEnumImpl () .Plain
        ) Token (_get_all_replacements (token ), token.parser_state, token.index )
        else token    )

  def _get_all_replacements (token: Token ): String =
    ReplacementImpl (token.text )
      .add_spaces_to_symbols (symbols = Translation () .SodaBracketsAndComma.toSet )
      .replace (ScalaNonSoda )
      .replace_at_beginning (token.index, SynonymAtBeginning )
      .replace (Synonym )
      .replace_with (try_definition )
      .replace_at_beginning (token.index, get_translation_table_at_beginning (token.text )  )
      .replace (MainTranslation )
      .replace_regex (Beautifier )
      .line

  def get_translation_table_at_beginning (line: String ): Translator =
    if (line.contains (SodaOpeningParenthesis )
    ) TranslationAtBeginningWithParen
    else TranslationAtBeginningWithoutParen

  def try_definition (line: String ): String =
    DefinitionTranslatorImpl (line ) .get_translation

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
}

case class MicroTranslatorImpl () extends MicroTranslator

trait DefaultTranslator extends soda.translator.replacement.Translator {

  def table: Seq [(String, String )]

  lazy val keys = table.map (pair => pair._1 )

  def translate (word: String ): String =
    table.toMap.get (word ) .getOrElse (word )
}

case class DefaultTranslatorImpl (table: Seq [(String, String )]  ) extends DefaultTranslator
