package soda.translator.extension.toscala

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

  lazy val tc = TranslationConstantToScala_ ()

  lazy val class_alias = BlockAnnotationEnum_ ().class_alias

  lazy val class_beginning = BlockAnnotationEnum_ ().class_beginning

  lazy val function_definition = BlockAnnotationEnum_ ().function_definition

  lazy val test_declaration = BlockAnnotationEnum_ ().test_declaration

  lazy val functions_and_tests = Seq (function_definition, test_declaration)

  lazy val class_declaration = Seq (class_alias, class_beginning)

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translation_pipeline.translate (block)

  lazy val try_definition : Token => String =
     token =>
      FunctionDefinitionLineTranslator_ (token.text).translation

  lazy val translation_pipeline =
    BlockTranslatorPipeline_ (
      Seq (
        MatchCaseBlockTranslator_ (),
        TokenReplacement_ ().replace (tc.scala_non_soda),
        TokenReplacement_ ().replace (tc.type_symbols_translation),
        ConditionalBlockTranslator_ (functions_and_tests, TokenizedBlockTranslator_ (try_definition) ),
        ConditionalBlockTranslator_ (functions_and_tests, TokenReplacement_ ().replace (tc.function_symbols_translation) ),
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
