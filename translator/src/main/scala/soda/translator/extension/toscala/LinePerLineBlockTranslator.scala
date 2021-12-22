package soda.translator.extension.toscala

trait LinePerLineBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.block.Translator
  import soda.translator.blocktr.TokenizedBlockTranslator_
  import soda.translator.blocktr.TableBlockTranslator_
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Token

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

  lazy val replace_token: Token => String =
     token =>
      Replacement_ (token.text )
        .add_spaces_to_symbols (symbols = tc.soda_brackets_and_comma.toSet )
        .replace (scala_non_soda )
        .replace_at_beginning (token.index, synonym_at_beginning )
        .replace (synonym )
        .replace_with (try_definition )
        .replace_at_beginning (token.index, get_translation_table_at_beginning (token.text )  )
        .replace (main_translation )
        .line

  lazy val translator = TokenizedBlockTranslator_ (replace_token )

  def translate (block: Block ): Block =
    translator.translate (block )

  def get_translation_table_at_beginning (line: String ): Translator =
    if (line.contains (soda_opening_parenthesis )
    ) translation_at_beginning_with_paren
    else
      if (DefinitionTranslatorToScala_ (line ) .condition_for_type_alias
      ) translation_at_beginning_without_paren_for_type_alias
      else translation_at_beginning_without_paren

  def try_definition (line: String ): String =
    DefinitionTranslatorToScala_ (line ) .translation

}

case class LinePerLineBlockTranslator_ ()  extends LinePerLineBlockTranslator
