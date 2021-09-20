package soda.translator.language


/**
 * This class translates Soda source code into Scala source code.
 */
trait MicroTranslator {
  import soda.lib.SomeSD_
  import soda.translator.replacement.CommentPreprocessor_
  import soda.translator.replacement.ParserStateEnum_
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Token
  import soda.translator.replacement.Token_
  import soda.translator.replacement.Tokenizer_
  import soda.translator.replacement.Translator

  lazy val tc = TranslationConstant_ ()

  lazy val new_line = "\n"

  lazy val soda_opening_parenthesis: String = "("

  lazy val synonym_at_beginning = DefaultTranslator_ (tc.synonym_at_beginning )

  lazy val translation_at_beginning_with_paren = DefaultTranslator_ (tc.translation_at_beginning_with_paren )

  lazy val translation_at_beginning_without_paren_for_type_alias =
      DefaultTranslator_ (tc.translation_at_beginning_without_paren_for_type_alias )

  lazy val translation_at_beginning_without_paren =
      DefaultTranslator_ (tc.translation_at_beginning_without_paren )

  lazy val synonym = DefaultTranslator_ (tc.synonym )

  lazy val main_translation = DefaultTranslator_ (tc.main_translation )

  lazy val scala_non_soda = DefaultTranslator_ (tc.scala_non_soda )

  lazy val beautifier = DefaultTranslator_ (tc.beautifier )

  def translate_program (program: String ): String =
    SomeSD_ (program )
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
    LineJoiner_ (lines ) .joined_lines_with_forward_join

  def join_lines_with_backward_join (lines: Seq [String]  ): Seq [String] =
    LineJoiner_ (lines ) .joined_lines_with_backward_join

  def join_translated_lines (lines: Seq [String]  ): String =
    lines.mkString (new_line ) + new_line

  def translate_lines (lines: Seq [String]  ): Seq [String] =
    CommentPreprocessor_ (lines )
      .annotated_lines
      .map (annotated_line =>
        if (annotated_line.isComment
        ) annotated_line.line
        else _translate_non_comment (annotated_line.line )      )

  def _translate_non_comment (line: String ): String =
      SomeSD_ (line )
        .map (x => Replacement_ (x ) .add_space_to_soda_line () .line )
        .map (x => Tokenizer_ (x ) .tokens )
        .map (x => _translate_line (x )  )
        .map (x => _join_tokens (x )  )
        .map (x => Replacement_ (x ) .remove_space_from_scala_line () .line )
        .value

  def _translate_line (tokens: Seq [Token]  ): Seq [Token] =
    tokens.map (token =>
        if (token.parser_state == ParserStateEnum_ () .plain
        ) Token_ (_get_all_replacements (token ), token.parser_state, token.index )
        else token    )

  def _get_all_replacements (token: Token ): String =
    Replacement_ (token.text )
      .add_spaces_to_symbols (symbols = tc.soda_brackets_and_comma.toSet )
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
    else
      if (DefinitionTranslator_ (line ) .condition_for_type_alias
      ) translation_at_beginning_without_paren_for_type_alias
      else translation_at_beginning_without_paren

  def try_definition (line: String ): String =
    DefinitionTranslator_ (line ) .translation

  def _join_tokens (tokens: Seq [Token]  ): String =
    tokens
      .map (token => token.text )
      .mkString ("")

  def preprocess_let_in_commands (lines: Seq [String]  ): Seq [String] =
    lines.map (line =>
      append_if_condition (line, starts_with_in, tc.scala_in_translation ) )

  def starts_with_in (line: String ): Boolean =
    line.trim () .startsWith (tc.soda_in_pattern )

  def append_if_condition (line: String, condition: String => Boolean, to_append: String ): String =
    if (condition (line )
    ) line + to_append
    else line
}

case class MicroTranslator_ () extends MicroTranslator

trait Table {

  def table: Seq [(String, String )]
}

trait DefaultTranslator  extends Table  with soda.translator.replacement.Translator {

  lazy val keys = table.map (pair => pair._1 )

  def translate (word: String ): String =
    table.toMap.get (word ) .getOrElse (word )
}

case class DefaultTranslator_ (table: Seq [(String, String )]  ) extends DefaultTranslator
