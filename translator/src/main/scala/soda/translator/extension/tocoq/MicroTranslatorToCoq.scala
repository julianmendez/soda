package soda.translator.extension.tocoq


/**
 * This class translates Soda snippets into Coq snippets.
 */
trait MicroTranslatorToCoq  extends soda.translator.block.BlockTranslator {

  import soda.lib.SomeSD_
  import soda.translator.block.Block
  import soda.translator.block.Block_
  import soda.translator.extension.toscala.DefaultTranslator_
  import soda.translator.replacement.CommentPreprocessor_
  import soda.translator.replacement.ParserStateEnum_
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Tokenizer_
  import soda.translator.replacement.Token
  import soda.translator.replacement.Token_

  lazy val source = "soda"

  lazy val target = "coq"

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val new_line = "\n"

  lazy val mtr = soda.translator.extension.toscala.MicroTranslatorToScala_ ()

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
      .map (mtr.join_lines_with_forward_join )
      .map (mtr.join_lines_with_backward_join )
      .map (translate_lines )
      .map (process_definition )
      .value

  def translate_lines (block: Block ): Block =
    Block_ (CommentPreprocessor_ (block.lines )
        .annotated_lines
        .map (annotated_line =>
          if (annotated_line.isComment
          ) annotated_line.line
          else _translate_non_comment (annotated_line.line )        )    )

  def process_definition (block: Block ): Block =
    if (is_a_definition (block )
    ) append (tc.coq_definition_end, prepend (tc.coq_definition, block ) )
    else block

  def prepend (prefix: String, block: Block ): Block =
    Block_ (Seq (prefix ) .++ (block.lines )    )

  def append (suffix: String, block: Block ): Block =
    Block_ (block.lines.:+ (suffix )    )

  def is_a_definition (block: Block ): Boolean =
    true
    /* FIXME: This should check whether the block is a definition. */

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

  def try_definition (line: String ): String =
    DefinitionTranslatorToCoq_ (line ) .translation

  def _join_tokens (tokens: Seq [Token]  ): String =
    tokens
      .map (token => token.text )
      .mkString ("")


  def _get_all_replacements (token: Token ): String =
    Replacement_ (token.text )
      .add_spaces_to_symbols (symbols = tc.soda_brackets_and_comma.toSet )
      .replace (scala_non_soda )
      .replace_at_beginning (token.index, synonym_at_beginning )
      .replace (synonym )
      .replace_with (try_definition )
      .replace (main_translation )
      .replace_regex (beautifier )
      .line

}

case class MicroTranslatorToCoq_ ()  extends MicroTranslatorToCoq
