package soda.translator.extension.tocoq

trait MatchCaseBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_
  import   soda.translator.parser.annotation.TestDeclarationAnnotation
  import   soda.translator.parser.annotation.TestDeclarationAnnotation_

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block )

  def translate_for (annotated_block: AnnotatedBlock ): AnnotatedBlock =
    annotated_block match  {
      case block: FunctionDefinitionAnnotation => _translate_function_block (block )
      case block: TestDeclarationAnnotation => _translate_test_block (block )
      case x => annotated_block
    }

  def _translate_function_block (block: AnnotatedBlock ): FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation_ (_translate_block (block ) )

  def _translate_test_block (block: AnnotatedBlock ): TestDeclarationAnnotation =
    TestDeclarationAnnotation_ (_translate_block (block ) )

  def _translate_block (block: AnnotatedBlock ): Block =
    BlockBuilder_ () .build (
      block.lines
        .map (line => append_with_after_match (line ) )
    )

  def append_with_after_match (line: String ): String =
    if (line.trim () .startsWith (tc.soda_match_pattern )
    ) line + tc.space + tc.coq_with_reserved_word
    else line

}

case class MatchCaseBlockTranslator_ ()
  extends
    MatchCaseBlockTranslator
{

}
