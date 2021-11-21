package soda.translator.language

/**
 * This class translates Soda source code into Scala source code.
 */
trait MicroTranslatorToScala  extends soda.translator.block.BlockTranslator {

  import soda.lib.SomeSD_
  import soda.translator.block.Block
  import soda.translator.block.Block_
  import soda.translator.replacement.CommentPreprocessor_
  import soda.translator.replacement.ParserStateEnum_
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Token
  import soda.translator.replacement.Token_
  import soda.translator.replacement.Tokenizer_
  import soda.translator.replacement.Translator

  lazy val source = "soda"

  lazy val target = "scala"

  lazy val tc = TranslationConstantToScala_ ()

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

  def translate (block: Block ): Block =
    SomeSD_ (block )
      .map (x => x.contents )
      .map (split_lines )
      .map (join_lines_with_forward_join )
      .map (preprocess_let_in_commands )
      .map (preprocess_match_case_commands )
      .map (join_lines_with_backward_join )
      .map (translate_lines )
      .map (join_translated_lines )
      .map (x => Block_ (x )  )
      .value

  def split_lines (block: String ): Seq [String] =
    block.split (new_line ) .toIndexedSeq

  def join_lines_with_forward_join (lines: Seq [String]  ): Seq [String] =
    LineJoinerToScala_ (lines ) .joined_lines_with_forward_join

  def join_lines_with_backward_join (lines: Seq [String]  ): Seq [String] =
    LineJoinerToScala_ (lines ) .joined_lines_with_backward_join

  def join_translated_lines (lines: Seq [String]  ): String =
    lines.mkString (new_line )

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
      if (DefinitionTranslatorToScala_ (line ) .condition_for_type_alias
      ) translation_at_beginning_without_paren_for_type_alias
      else translation_at_beginning_without_paren

  def try_definition (line: String ): String =
    DefinitionTranslatorToScala_ (line ) .translation

  def _join_tokens (tokens: Seq [Token]  ): String =
    tokens
      .map (token => token.text )
      .mkString ("")

  def preprocess_let_in_commands (lines: Seq [String]  ): Seq [String] =
    lines
      .map (line => replace_all_when (line, starts_with, tc.soda_in_let_pattern, tc.scala_in_let_translation ) )
      .map (line => replace_all_when (line, are_trim_equal, tc.soda_in_let_pattern.trim, tc.scala_in_let_translation ) )
      .map (line => append_if_condition (line, starts_with, tc.soda_in_pattern, tc.scala_in_translation ) )

  def preprocess_match_case_commands (lines: Seq [String]  ): Seq [String] =
    lines.map (line =>
       insert_match_before_brace_if_found (line ) )

  def starts_with (line: String, pattern: String ): Boolean =
    line.trim.startsWith (pattern )

  def are_trim_equal (line: String, pattern: String ): Boolean =
    (line.trim == pattern.trim )

  def replace_all_when (line: String, condition: (String, String ) => Boolean, pattern: String, new_text: String ): String =
    if (condition (line, pattern )
    ) line.replaceAll (pattern, new_text )
    else line

  def append_if_condition (line: String, condition: (String, String ) => Boolean, pattern: String, to_append: String ): String =
    if (condition (line, pattern )
    ) line + to_append
    else line

  def insert_match_before_brace_if_found (line: String ): String =
    if (line.trim () .startsWith (tc.soda_match_pattern )
    )
      {
        lazy val index_of_match = line.indexOf (tc.soda_match_pattern )
        lazy val left_part = line.substring (0, index_of_match )
        lazy val right_part = line.substring (index_of_match + tc.soda_match_pattern.length, line.length )
        left_part + insert_match_before_brace (right_part ) }
    else line

  def insert_match_before_brace (line: String ): String =
    {
      lazy val index_of_brace = line.indexOf (tc.soda_opening_brace )
      lazy val result =
        if (index_of_brace >= 0
        ) line.substring (0, index_of_brace ) + tc.scala_match_translation + line.substring (index_of_brace, line.length )
        else line + tc.scala_match_translation
      result }

}

case class MicroTranslatorToScala_ ()  extends MicroTranslatorToScala

trait Table {

  def table: Seq [(String, String )]

}

trait DefaultTranslator  extends Table  with soda.translator.replacement.Translator {

  lazy val keys = table.map (pair => pair._1 )

  def translate (word: String ): String =
    table.toMap.get (word ) .getOrElse (word )

}

case class DefaultTranslator_ (table: Seq [(String, String )]  )  extends DefaultTranslator


