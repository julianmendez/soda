package soda.translator.extension.toscala

/**
 * This class translates Soda source code into Scala source code.
 */
trait MicroTranslatorToScala  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.block.BlockTranslatorPipeline_
  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.block.ConditionalBlockTranslator_
  import soda.translator.blocktr.TokenReplacement_
  import soda.translator.blocktr.TokenizedBlockTranslator_
  import soda.translator.parser.BlockAnnotator_
  import soda.translator.replacement.Token

  lazy val new_line = "\n"

  lazy val tc = TranslationConstantToScala_ ()

  lazy val function_definition = BlockAnnotationEnum_ () .function_definition

  lazy val test_declaration = BlockAnnotationEnum_ () .test_declaration

  lazy val try_definition: Token => String =
     token =>
      FunctionDefinitionLineTranslator_ (token.text ) .translation

  lazy val translation_pipeline =
    BlockTranslatorPipeline_ (Seq (BlockAnnotator_ (), LetInBlockTranslator_ (), MatchCaseBlockTranslator_ (), TokenReplacement_ () .add_spaces_to_symbols (symbols = tc.soda_brackets_and_comma.toSet ), TokenReplacement_ () .replace (tc.scala_non_soda ), TokenReplacement_ () .replace_at_beginning (tc.synonym_at_beginning ), TokenReplacement_ () .replace (tc.synonym ), ConditionalBlockTranslator_ (function_definition, TokenizedBlockTranslator_ (try_definition ) ), ConditionalBlockTranslator_ (test_declaration, TokenizedBlockTranslator_ (try_definition ) ), ClassDeclarationBlockTranslator_ (), TokenReplacement_ () .replace (tc.main_translation ), TheoremAndProofBlockTranslator_ (), TokenReplacement_ () .replace_regex (tc.beautifier )      )    )

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    translation_pipeline.translate (block )

}

case class MicroTranslatorToScala_ ()  extends MicroTranslatorToScala
