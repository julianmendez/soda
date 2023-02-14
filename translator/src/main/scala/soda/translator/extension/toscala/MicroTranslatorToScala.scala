package soda.translator.extension.toscala

/*
 * This package contains classes for the translation to Scala.
 */





/**
 * This class translates Soda source code into Scala source code.
 */

trait MicroTranslatorToScala
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockTranslatorPipeline_
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.ConditionalBlockTranslator_
  import   soda.translator.blocktr.TokenReplacement_
  import   soda.translator.blocktr.TokenizedBlockTranslator_
  import   soda.translator.replacement.Token

  private lazy val _tc = TranslationConstantToScala_ ()

  private lazy val _ba = BlockAnnotationEnum_ ()

  private lazy val _functions_and_tests =
    Seq (_ba.function_definition, _ba.test_declaration)

  private lazy val _class_declarations =
    Seq (_ba.class_alias, _ba.class_beginning, _ba.abstract_declaration)

  private lazy val _definitions_and_declarations =
    _functions_and_tests.++ (_class_declarations)

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _translation_pipeline.translate (block)

  private lazy val _translation_pipeline =
    BlockTranslatorPipeline_ (
      Seq (
        MatchCaseBlockTranslator_ (),
        ConditionalBlockTranslator_ (_definitions_and_declarations, TokenReplacement_ ().replace (_tc.scala_non_soda) ),
        ConditionalBlockTranslator_ (_functions_and_tests, FunctionDefinitionBlockTranslator_ () ),
        ConditionalBlockTranslator_ (_class_declarations, TokenReplacement_ ().replace (_tc.type_symbol_translation) ),
        ConditionalBlockTranslator_ (_functions_and_tests, TokenReplacement_ ().replace (_tc.all_translations) ),
        ClassDeclarationBlockTranslator_ (),
        ImportDeclarationBlockTranslator_ (),
        AbstractDeclarationBlockTranslator_ (),
        TheoremAndProofBlockTranslator_ (),
        ClassEndBlockTranslator_ (),
        MainClassBlockTranslator_ (),
        ClassConstructorBlockTranslator_ ()
      )
    )

}

case class MicroTranslatorToScala_ () extends MicroTranslatorToScala
