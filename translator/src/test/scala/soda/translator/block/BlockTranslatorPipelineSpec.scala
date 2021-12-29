package soda.translator.block

case class BlockTranslator00 ()  extends BlockTranslator {

  import soda.translator.parser.BlockBuilder_

  lazy val source: String = "00"

  lazy val target: String = "01"

  def translate (block: Block ): Block =
    BlockBuilder_ () .build (if (block.lines.isEmpty
      ) Seq ("")
      else block.lines.++ (Seq ("tr00") )    )

}

case class BlockTranslator01 ()  extends BlockTranslator {

  import soda.translator.parser.BlockBuilder_

  lazy val source: String = "01"

  lazy val target: String = "02"

  def translate (block: Block ): Block =
    BlockBuilder_ () .build (if (block.lines.isEmpty
      ) Seq ("")
      else block.lines.++ (Seq ("tr01") )    )

}

case class BlockTranslator02 ()  extends BlockTranslator {

  import soda.translator.parser.BlockBuilder_

  lazy val source: String = "02"

  lazy val target: String = "03"

  def translate (block: Block ): Block =
    BlockBuilder_ () .build (if (block.lines.isEmpty
      ) Seq ("")
      else block.lines.++ (Seq ("tr02") )    )

}

case class BlockTranslatorPipelineSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.translator.parser.BlockBuilder_

  lazy val instance =
    BlockTranslatorPipeline_ (Seq (BlockTranslator00 (), BlockTranslator01 (), BlockTranslator02 ()      )    )

  test ("block translator pipeline")
    {
      lazy val original = BlockBuilder_ () .build (Seq ("first line" )      )
      lazy val expected =
        BlockBuilder_ () .build (Seq ("first line", "tr00", "tr01", "tr02"          )        )
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

}
