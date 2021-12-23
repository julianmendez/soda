package soda.translator.extension.tocoq

trait LinePerLineBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.blocktr.TableTranslator_
  import soda.translator.blocktr.TokenizedBlockTranslator_
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Token

  lazy val source = "soda"

  lazy val target = "coq"

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val new_line = "\n"

  lazy val space = " "

  lazy val soda_opening_parenthesis: String = "("

  lazy val synonym_at_beginning = TableTranslator_ (tc.synonym_at_beginning )

  lazy val synonym = TableTranslator_ (tc.synonym )

  lazy val main_translation = TableTranslator_ (tc.main_translation )

  lazy val coq_non_soda = TableTranslator_ (tc.coq_non_soda )

  lazy val translator = TokenizedBlockTranslator_ (replace_token )

  def translate (block: Block ): Block =
    translator.translate (block )

  lazy val replace_token: Token => String =
     token =>
      Replacement_ (token.text )
        .add_spaces_to_symbols (symbols = tc.soda_brackets_and_comma.toSet )
        .replace (coq_non_soda )
        .replace_at_beginning (token.index, synonym_at_beginning )
        .replace (synonym )
        .replace_with (try_definition )
        .line

  def try_definition (line: String ): String =
    DefinitionTranslatorToCoq_ (line ) .translation

}

case class LinePerLineBlockTranslator_ ()  extends LinePerLineBlockTranslator
