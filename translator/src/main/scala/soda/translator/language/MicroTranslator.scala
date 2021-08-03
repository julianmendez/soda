package soda.translator.language


/**
 * This class translates Soda source code into Scala source code.
 */
trait MicroTranslator {
  import soda.lib.SomeElem
  import soda.translator.replacement.CommentPreprocessorImpl
  import soda.translator.replacement.ParserStateEnum
  import soda.translator.replacement.ReplacementImpl
  import soda.translator.replacement.Token
  import soda.translator.replacement.TokenizerImpl
  import soda.translator.replacement.Translator

  lazy val new_line = "\n"

  lazy val soda_opening_parenthesis: String = "("

  lazy val synonym_at_beginning = DefaultTranslatorImpl (Translation () .synonym_at_beginning )

  lazy val translation_at_beginning_with_paren = DefaultTranslatorImpl (Translation () .translation_at_beginning_with_paren )

  lazy val translation_at_beginning_without_paren = DefaultTranslatorImpl (Translation () .translation_at_beginning_without_paren )

  lazy val synonym = DefaultTranslatorImpl (Translation () .synonym )

  lazy val main_translation = DefaultTranslatorImpl (Translation () .main_translation )

  lazy val scala_non_soda = DefaultTranslatorImpl (Translation () .scala_non_soda )

  lazy val beautifier = DefaultTranslatorImpl (Translation () .beautifier )

  def translate_program (program: String ): String =
    SomeElem (program )
      .map (split_lines )
      .map (join_lines_with_forward_join )
      .map (preprocess_let_in_commands )
      .map (join_lines_with_backward_join )
      .map (translate_lines )
      .map (join_translated_lines )
      .value

  def split_lines (program: String ): Seq [String] =
    program.split (new_line ) .toIndexedSeq

  def join_lines_with_forward_join (lines: Seq [String]  ): Seq [String] =
    LineJoinerImpl (lines ) .joined_lines_with_forward_join

  def join_lines_with_backward_join (lines: Seq [String]  ): Seq [String] =
    LineJoinerImpl (lines ) .joined_lines_with_backward_join

  def join_translated_lines (lines: Seq [String]  ): String =
    lines.mkString (new_line ) + new_line

  def translate_lines (lines: Seq [String]  ): Seq [String] =
    CommentPreprocessorImpl (lines )
      .annotated_lines
      .map (annotated_line =>
        if (annotated_line.isComment
        ) annotated_line.line
        else _translate_non_comment (annotated_line.line )      )

  def _translate_non_comment (line: String ): String =
      SomeElem (line )
        .map (x => ReplacementImpl (x ) .add_space_to_soda_line () .line )
        .map (x => TokenizerImpl (x ) .tokens )
        .map (x => _translate_line (x )  )
        .map (x => _join_tokens (x )  )
        .map (x => ReplacementImpl (x ) .remove_space_from_scala_line () .line )
        .value

  def _translate_line (tokens: Seq [Token]  ): Seq [Token] =
    tokens.map (token =>
        if (token.parser_state == ParserStateEnum () .plain
        ) Token (_get_all_replacements (token ), token.parser_state, token.index )
        else token    )

  def _get_all_replacements (token: Token ): String =
    ReplacementImpl (token.text )
      .add_spaces_to_symbols (symbols = Translation () .soda_brackets_and_comma.toSet )
      .replace (scala_non_soda )
      .replace_at_beginning (token.index, synonym_at_beginning )
      .replace (synonym )
      .replace_with (try_definition )
      .replace_at_beginning (token.index, get_translation_table_at_beginning (token.text )  )
      .replace (main_translation )
      .replace_regex (beautifier )
      .line

  def get_translation_table_at_beginning (line: String ): Translator =
    if (line.contains (soda_opening_parenthesis )
    ) translation_at_beginning_with_paren
    else translation_at_beginning_without_paren

  def try_definition (line: String ): String =
    DefinitionTranslatorImpl (line ) .translation

  def _join_tokens (tokens: Seq [Token]  ): String =
    tokens
      .map (token => token.text )
      .mkString ("")

  def preprocess_let_in_commands (lines: Seq [String]  ): Seq [String] =
    lines.map (line =>
      append_if_condition (line, starts_with_in, Translation () .scala_in_translation ) )

  def starts_with_in (line: String ): Boolean =
    line.trim () .startsWith (Translation () .soda_in_pattern )

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
