package soda.translator.block

trait BlockTranslatorPipeline  extends BlockTranslator {

  import soda.lib.Recursion_

  def pipeline: Seq [BlockTranslator]

  lazy val source: String =
    if (pipeline.isEmpty
    ) ""
    else pipeline.head.source

  lazy val target: String =
    if (pipeline.isEmpty
    ) ""
    else pipeline.last.target

  def translate (block: Block ): Block =
    Recursion_ ()
      .fold (pipeline, block, _next_value_function )

  def _next_value_function (block: Block, translator: BlockTranslator ): Block =
    translator.translate (block )

}

case class BlockTranslatorPipeline_ (pipeline: Seq [BlockTranslator]  )  extends BlockTranslatorPipeline
