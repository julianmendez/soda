package soda.translator.extension.tocoq

/**
 * This class translates Soda snippets into Coq snippets.
 */
trait MicroTranslatorToCoq  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.block.BlockTranslatorPipeline_
  import soda.translator.blocktr.LineBackwardJoinerBlockTranslator_
  import soda.translator.blocktr.LineForwardJoinerBlockTranslator_
  import soda.translator.blocktr.TableTranslator_
  import soda.translator.blocktr.TokenizedBlockTranslator_
  import soda.translator.replacement.ReplacementAux_
  import soda.translator.replacement.Token

  lazy val source = "soda"

  lazy val target = "coq"

  lazy val tc = TranslationConstantToCoq_ ()

  def replace (table: Seq [(String, String )] ): Token => String =
     token =>
        ReplacementAux_ () .replace (token.text, TableTranslator_ (table )  )

  def replace_regex (table: Seq [(String, String )] ): Token => String =
     token =>
        ReplacementAux_ () .replace_regex (token.text, TableTranslator_ (table )  )

  lazy val translation_pipeline =
    BlockTranslatorPipeline_ (Seq (LineForwardJoinerBlockTranslator_ (), LineBackwardJoinerBlockTranslator_ (), MatchCaseBlockTranslator_ (), LinePerLineBlockTranslator_ (), TokenizedBlockTranslator_ (replace (tc.main_translation ) ), TokenizedBlockTranslator_ (replace_regex (tc.beautifier ) ), DefinitionBlockTranslator_ ()      )    )

  def translate (block: Block ): Block =
    translation_pipeline.translate (block )

}

case class MicroTranslatorToCoq_ ()  extends MicroTranslatorToCoq
