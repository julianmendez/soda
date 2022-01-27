package soda.translator.block

case class BlockTranslator00 ()
  extends
    BlockTranslator
{

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.BlockBuilder_

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      if (block.lines.isEmpty
      ) Seq ("")
      else block.lines.++ (Seq ("tr00") ),
      BlockAnnotationEnum_ () .undefined
    )

}

case class BlockTranslator01 ()
  extends
    BlockTranslator
{

  import   soda.translator.parser.BlockBuilder_

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      if (block.lines.isEmpty
      ) Seq ("")
      else block.lines.++ (Seq ("tr01") ),
      BlockAnnotationEnum_ () .undefined
    )

}

case class BlockTranslator02 ()
  extends
    BlockTranslator
{

  import   soda.translator.parser.BlockBuilder_

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      if (block.lines.isEmpty
      ) Seq ("")
      else block.lines.++ (Seq ("tr02") ),
      BlockAnnotationEnum_ () .undefined
    )

}

case class BlockTranslatorPipelineSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.parser.BlockBuilder_

  lazy val instance =
    BlockTranslatorPipeline_ (
      Seq (
        BlockTranslator00 (),
        BlockTranslator01 (),
        BlockTranslator02 ()
      )
    )

  test ("block translator pipeline")
    {
      lazy val original = BlockBuilder_ () .build (
        Seq ("first line" ),
        BlockAnnotationEnum_ () .undefined
      )
      lazy val expected =
        BlockBuilder_ () .build (
          Seq (
            "first line",
            "tr00",
            "tr01",
            "tr02"
          ),
          BlockAnnotationEnum_ () .undefined
        )
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

}
