package soda.translator.extension.todoc

/*
 * This package contains classes for documentation generation.
 */





/**
 * This class generates documentation from Soda snippets.
 */

trait MicroTranslatorToDoc
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.BlockTranslatorPipeline_
  import   soda.translator.block.ConditionalBlockTranslator_
  import   soda.translator.blocktr.TokenReplacement_
  import   soda.translator.blocktr.TokenizedBlockTranslator_
  import   soda.translator.replacement.Token

  private lazy val _tc = TranslationConstantToDoc_ ()

  private lazy val _function_definition = BlockAnnotationEnum_ ().function_definition

  private lazy val _test_declaration = BlockAnnotationEnum_ ().test_declaration

  lazy val functions_and_tests = Seq (_function_definition, _test_declaration)

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _translation_pipeline.translate (block)

  private lazy val _translation_pipeline =
    BlockTranslatorPipeline_ (
      Seq (
        DocBlockTranslator_ ()
      )
    )

}

case class MicroTranslatorToDoc_ () extends MicroTranslatorToDoc
