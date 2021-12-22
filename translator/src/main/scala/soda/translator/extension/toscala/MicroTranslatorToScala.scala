package soda.translator.extension.toscala

/**
 * This class translates Soda source code into Scala source code.
 */
trait MicroTranslatorToScala  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.block.Block_
  import soda.translator.block.BlockTranslatorPipeline_
  import soda.translator.blocktr.LineBackwardJoinerBlockTranslator_
  import soda.translator.blocktr.LineForwardJoinerBlockTranslator_

  lazy val source = "soda"

  lazy val target = "scala"

  lazy val new_line = "\n"

  lazy val translation_pipeline =
    BlockTranslatorPipeline_ (Seq (LineForwardJoinerBlockTranslator_ (), LetInBlockTranslator_ (), MatchCaseBlockTranslator_ (), LineBackwardJoinerBlockTranslator_ (), LinePerLineBlockTranslator_ (), BeautifierBlockTranslator_ ()      )    )

  def translate (block: Block ): Block =
    translation_pipeline.translate (block )

}

case class MicroTranslatorToScala_ ()  extends MicroTranslatorToScala
