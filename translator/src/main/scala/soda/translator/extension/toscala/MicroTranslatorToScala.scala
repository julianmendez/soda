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

  lazy val ba = BlockAnnotationEnum_ ()

  lazy val functions_and_tests =
    Seq (ba.function_definition, ba.test_declaration)

  lazy val class_declarations =
    Seq (ba.class_alias, ba.class_beginning, ba.abstract_declaration)

  lazy val definitions_and_declarations =
    functions_and_tests.++ (class_declarations)

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
        ConditionalBlockTranslator_ (definitions_and_declarations, TokenReplacement_ ().replace (tc.scala_non_soda) ),
        ConditionalBlockTranslator_ (functions_and_tests, TokenizedBlockTranslator_ (try_definition) ),
        ConditionalBlockTranslator_ (class_declarations, TokenReplacement_ ().replace (tc.type_symbol_translation) ),
        ConditionalBlockTranslator_ (functions_and_tests, TokenReplacement_ ().replace (tc.all_translations) ),
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
