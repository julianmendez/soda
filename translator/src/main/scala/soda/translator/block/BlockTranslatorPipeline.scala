package soda.translator.block

trait BlockTranslatorPipeline
  extends
    BlockTranslator
{

  import   soda.lib.Fold_

  def   pipeline : Seq [BlockTranslator]

  private lazy val _fold = Fold_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _fold.apply (pipeline) (block) (_next_value_function)

  private def _next_value_function (block : AnnotatedBlock) (translator : BlockTranslator) : AnnotatedBlock =
    translator.translate (block)

}

case class BlockTranslatorPipeline_ (pipeline : Seq [BlockTranslator]) extends BlockTranslatorPipeline
