package soda.translator.extension.tocoq

/**
 * This class translates Soda snippets into Coq snippets.
 */
trait MicroTranslatorToCoq  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.block.BlockTranslatorPipeline_
  import soda.translator.blocktr.LineBackwardJoinerBlockTranslator_
  import soda.translator.blocktr.LineForwardJoinerBlockTranslator_
  import soda.translator.blocktr.TokenReplacement_
  import soda.translator.blocktr.TokenizedBlockTranslator_
  import soda.translator.replacement.Token

  lazy val source = "soda"

  lazy val target = "coq"

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val try_definition: Token => String =
     token =>
      DefinitionLineTranslator_ (token.text ) .translation

  lazy val translation_pipeline =
    BlockTranslatorPipeline_ (Seq (LineForwardJoinerBlockTranslator_ (), LineBackwardJoinerBlockTranslator_ (), MatchCaseBlockTranslator_ (), TokenReplacement_ () .add_spaces_to_symbols (symbols = tc.soda_brackets_and_comma.toSet ), TokenReplacement_ () .replace (tc.coq_non_soda ), TokenReplacement_ () .replace_at_beginning (tc.synonym_at_beginning ), TokenReplacement_ () .replace (tc.synonym ), TokenizedBlockTranslator_ (try_definition ), TokenReplacement_ () .replace (tc.main_translation ), TokenReplacement_ () .replace_regex (tc.beautifier ), CoqDefinitionBlockTranslator_ ()      )    )

  def translate (block: Block ): Block =
    translation_pipeline.translate (block )

}

case class MicroTranslatorToCoq_ ()  extends MicroTranslatorToCoq
