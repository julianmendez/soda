package soda.translator.extension.toscala

trait LinePerLineBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.lib.SomeSD_
  import soda.translator.block.Block
  import soda.translator.block.Block_
  import soda.translator.block.Translator
  import soda.translator.blocktr.TableBlockTranslator_
  import soda.translator.replacement.ParserStateEnum_
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Token
  import soda.translator.replacement.Token_
  import soda.translator.replacement.Tokenizer_

  lazy val source = "soda"

  lazy val target = "soda"

  lazy val soda_opening_parenthesis: String = "("

  lazy val tc = TranslationConstantToScala_ ()

  lazy val synonym_at_beginning = TableBlockTranslator_ (tc.synonym_at_beginning )

  lazy val translation_at_beginning_with_paren = TableBlockTranslator_ (tc.translation_at_beginning_with_paren )

  lazy val translation_at_beginning_without_paren_for_type_alias =
      TableBlockTranslator_ (tc.translation_at_beginning_without_paren_for_type_alias )

  lazy val translation_at_beginning_without_paren =
      TableBlockTranslator_ (tc.translation_at_beginning_without_paren )

  lazy val synonym = TableBlockTranslator_ (tc.synonym )

  lazy val main_translation = TableBlockTranslator_ (tc.main_translation )

  lazy val scala_non_soda = TableBlockTranslator_ (tc.scala_non_soda )

  lazy val beautifier = TableBlockTranslator_ (tc.beautifier )

  def translate (block: Block ): Block =
    Block_ (block
        .annotated_lines
        .map (annotated_line =>
          if (annotated_line.isComment
          ) annotated_line.line
          else _translate_non_comment (annotated_line.line )        )    )

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

}

case class LinePerLineBlockTranslator_ ()  extends LinePerLineBlockTranslator
