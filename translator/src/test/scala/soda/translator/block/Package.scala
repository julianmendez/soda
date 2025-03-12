package soda.translator.block

/*
 * This package contains tests for block translators.
 */



trait BlockTranslator00
  extends
    BlockTranslator
{



  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.AnnotationFactory

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      AnnotationFactory .mk .annotate (
        BlockBuilder .mk .build (
          if ( block .lines .isEmpty
          ) Seq ("")
          else block .lines .++ (Seq ("tr00") )
        )
      )

}

case class BlockTranslator00_ () extends BlockTranslator00

object BlockTranslator00 {
  def mk : BlockTranslator00 =
    BlockTranslator00_ ()
}

trait BlockTranslator01
  extends
    BlockTranslator
{



  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.AnnotationFactory

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      AnnotationFactory .mk .annotate (
        BlockBuilder .mk .build (
          if ( block .lines .isEmpty
          ) Seq ("")
          else block .lines .++ (Seq ("tr01") )
        )
      )

}

case class BlockTranslator01_ () extends BlockTranslator01

object BlockTranslator01 {
  def mk : BlockTranslator01 =
    BlockTranslator01_ ()
}

trait BlockTranslator02
  extends
    BlockTranslator
{



  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.AnnotationFactory

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      AnnotationFactory .mk .annotate (
        BlockBuilder .mk .build (
          if ( block .lines .isEmpty
          ) Seq ("")
          else block .lines .++ (Seq ("tr02") )
        )
      )

}

case class BlockTranslator02_ () extends BlockTranslator02

object BlockTranslator02 {
  def mk : BlockTranslator02 =
    BlockTranslator02_ ()
}

case class BlockTranslatorPipelineSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.AnnotationFactory

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance =
    BlockTranslatorPipeline .mk (
      Seq (
        BlockTranslator00 .mk ,
        BlockTranslator01 .mk ,
        BlockTranslator02 .mk
      )
    )

  lazy val original =
    AnnotationFactory .mk .annotate (
      BlockBuilder .mk .build (
        Seq ("first line" )
      )
    )

  test ("block translator pipeline") (
    check (
      obtained = instance .translate (original)
    ) (
      expected =
        AnnotationFactory .mk .annotate (
          BlockBuilder .mk .build (
            Seq (
              "first line" ,
              "tr00" ,
              "tr01" ,
              "tr02"
            )
          )
        )
    )
  )

}

