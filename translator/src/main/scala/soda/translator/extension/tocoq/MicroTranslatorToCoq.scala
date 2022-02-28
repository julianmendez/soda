package soda.translator.extension.tocoq

/**
 * This class translates Soda snippets into Coq snippets.
 */

trait MicroTranslatorToCoq
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.BlockTranslatorPipeline_
  import   soda.translator.block.ConditionalBlockTranslator_
  import   soda.translator.blocktr.TokenReplacement_
  import   soda.translator.blocktr.TokenizedBlockTranslator_
  import   soda.translator.parser.annotation.AnnotationFactory_
  import   soda.translator.replacement.Token

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val function_definition = BlockAnnotationEnum_ ().function_definition

  lazy val test_declaration = BlockAnnotationEnum_ ().test_declaration

  lazy val functions_and_tests = Seq (function_definition, test_declaration)

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translation_pipeline.translate (block)

  lazy val try_definition : Token => String =
     token =>
      DefinitionLineTranslator_ (token.text).translation

  lazy val translation_pipeline =
    BlockTranslatorPipeline_ (
      Seq (
        MatchCaseBlockTranslator_ (),
        CoqDefinitionBlockTranslator_ (),
        CoqClassConstructorBlockTranslator_ (),
        CoqClassDeclarationBlockTranslator_ (),
        CoqTheoremBlockTranslator_ (),
        CoqProofBlockTranslator_ (),
        ConditionalBlockTranslator_ (functions_and_tests, TokenizedBlockTranslator_ (try_definition) ),
        ConditionalBlockTranslator_ (functions_and_tests, TokenReplacement_ ().replace (tc.function_symbols_translation) )
      )
    )

}

case class MicroTranslatorToCoq_ () extends MicroTranslatorToCoq
