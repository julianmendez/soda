package soda.translator.block

/*
 * This package contains tests for block translators.
 */

trait BlockTranslator00
  extends
    BlockTranslator
{

  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.AnnotationFactory_

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      AnnotationFactory_ ().annotate (
        BlockBuilder_ ().build (
          if ( block.lines.isEmpty
          ) Seq ("")
          else block.lines.++ ( Seq ("tr00") )
        )
      )

}

case class BlockTranslator00_ () extends BlockTranslator00

trait BlockTranslator01
  extends
    BlockTranslator
{

  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.AnnotationFactory_

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      AnnotationFactory_ ().annotate (
        BlockBuilder_ ().build (
          if ( block.lines.isEmpty
          ) Seq ("")
          else block.lines.++ ( Seq ("tr01") )
        )
      )

}

case class BlockTranslator01_ () extends BlockTranslator01

trait BlockTranslator02
  extends
    BlockTranslator
{

  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.AnnotationFactory_

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      AnnotationFactory_ ().annotate (
        BlockBuilder_ ().build (
          if ( block.lines.isEmpty
          ) Seq ("")
          else block.lines.++ ( Seq ("tr02") )
        )
      )

}

case class BlockTranslator02_ () extends BlockTranslator02

case class BlockTranslatorPipelineSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.AnnotationFactory_

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance =
    BlockTranslatorPipeline_ (
      Seq (
        BlockTranslator00_ (),
        BlockTranslator01_ (),
        BlockTranslator02_ ()
      )
    )

  lazy val original =
    AnnotationFactory_ ().annotate (
      BlockBuilder_ ().build (
        Seq ( "first line" )
      )
    )

  test ("block translator pipeline") (
    check (
      obtained = instance.translate (original)
    ) (
      expected =
        AnnotationFactory_ ().annotate (
          BlockBuilder_ ().build (
            Seq (
              "first line",
              "tr00",
              "tr01",
              "tr02"
            )
          )
        )
    )
  )

}
