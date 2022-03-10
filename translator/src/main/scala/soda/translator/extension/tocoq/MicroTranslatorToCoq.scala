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
  import   soda.translator.replacement.Token

  private lazy val _tc = TranslationConstantToCoq_ ()

  private lazy val _function_definition = BlockAnnotationEnum_ ().function_definition

  private lazy val _test_declaration = BlockAnnotationEnum_ ().test_declaration

  lazy val functions_and_tests = Seq (_function_definition, _test_declaration)

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _translation_pipeline.translate (block)

  lazy val try_definition : Token => String =
     token =>
      DefinitionLineTranslator_ (token.text).translation

  private lazy val _translation_pipeline =
    BlockTranslatorPipeline_ (
      Seq (
        MatchCaseBlockTranslator_ (),
        CoqDefinitionBlockTranslator_ (),
        CoqClassConstructorBlockTranslator_ (),
        CoqClassDeclarationBlockTranslator_ (),
        CoqPackageDeclarationBlockTranslator_ (),
        CoqClassEndBlockTranslator_ (),
        CoqImportDeclarationBlockTranslator_ (),
        CoqTheoremBlockTranslator_ (),
        CoqProofBlockTranslator_ (),
        ConditionalBlockTranslator_ (functions_and_tests, TokenizedBlockTranslator_ (try_definition) ),
        ConditionalBlockTranslator_ (functions_and_tests, TokenReplacement_ ().replace (_tc.function_symbols_translation) )
      )
    )

}

case class MicroTranslatorToCoq_ () extends MicroTranslatorToCoq
