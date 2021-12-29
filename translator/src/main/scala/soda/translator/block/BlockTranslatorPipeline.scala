package soda.translator.block

trait BlockTranslatorPipeline  extends BlockTranslator {

  import soda.lib.Recursion_

  def pipeline: Seq [BlockTranslator]

  def translate (block: Block ): Block =
    Recursion_ ()
      .fold (pipeline, block, _next_value_function )

  def _next_value_function (block: Block, translator: BlockTranslator ): Block =
    translator.translate (block )

}

case class BlockTranslatorPipeline_ (pipeline: Seq [BlockTranslator]  )  extends BlockTranslatorPipeline
