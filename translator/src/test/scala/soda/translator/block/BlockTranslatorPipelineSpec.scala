package soda.translator.block

case class BlockTranslator00 ()  extends BlockTranslator {

  lazy val source: String = "00"

  lazy val target: String = "01"

  def translate (block: Block ): Block =
    Block_ (if (block.lines.isEmpty
      ) Seq ("")
      else block.lines.++ (Seq ("tr00") )    )

}

case class BlockTranslator01 ()  extends BlockTranslator {

  lazy val source: String = "01"

  lazy val target: String = "02"

  def translate (block: Block ): Block =
    Block_ (if (block.lines.isEmpty
      ) Seq ("")
      else block.lines.++ (Seq ("tr01") )    )

}

case class BlockTranslator02 ()  extends BlockTranslator {

  lazy val source: String = "02"

  lazy val target: String = "03"

  def translate (block: Block ): Block =
    Block_ (if (block.lines.isEmpty
      ) Seq ("")
      else block.lines.++ (Seq ("tr02") )    )

}

case class BlockTranslatorPipelineSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.translator.block.BlockProcessor_

  lazy val instance =
    BlockTranslatorPipeline_ (Seq (BlockTranslator00 (), BlockTranslator01 (), BlockTranslator02 ()      )    )

  test ("block translator pipeline")
    {
      lazy val original = Block_ (Seq ("first line" )      )
      lazy val expected =
        Block_ (Seq ("first line", "tr00", "tr01", "tr02"          )        )
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

}
