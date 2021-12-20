package soda.translator.extension.tocoq

/**
 * This class translates Soda snippets into Coq snippets.
 */
trait MicroTranslatorToCoq  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.block.BlockTranslatorPipeline_
  import soda.translator.blocktr.LineBackwardJoinerBlockTranslator_
  import soda.translator.blocktr.LineForwardJoinerBlockTranslator_

  lazy val source = "soda"

  lazy val target = "coq"

  lazy val translation_pipeline =
    BlockTranslatorPipeline_ (Seq (LineForwardJoinerBlockTranslator_ (), LineBackwardJoinerBlockTranslator_ (), MatchCaseBlockTranslator_ (), LinePerLineBlockTranslator_ (), DefinitionBlockTranslator_ ()      )    )

  def translate (block: Block ): Block =
    translation_pipeline.translate (block )

}

case class MicroTranslatorToCoq_ ()  extends MicroTranslatorToCoq
