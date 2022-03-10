package soda.translator.extension.tocoq

trait MatchCaseBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_
  import   soda.translator.parser.annotation.TestDeclarationAnnotation
  import   soda.translator.parser.annotation.TestDeclarationAnnotation_
  import   soda.translator.replacement.ReplacementAux_

  lazy val sc = SodaConstant_ ()

  lazy val soda_case_pattern = sc.case_reserved_word + sc.space

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val soda_match_pattern = sc.match_reserved_word + " "

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case FunctionDefinitionAnnotation_ (block) => _translate_function_block (FunctionDefinitionAnnotation_ (block) )
      case TestDeclarationAnnotation_ (block) => _translate_test_block (TestDeclarationAnnotation_ (block) )
      case x => annotated_block
    }

  private def _translate_function_block (block : AnnotatedBlock) : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation_ (_translate_block (block) )

  private def _translate_test_block (block : AnnotatedBlock) : TestDeclarationAnnotation =
    TestDeclarationAnnotation_ (_translate_block (block) )

  private def _translate_block (block : AnnotatedBlock) : Block =
    BlockBuilder_ ().build(
      block.lines
        .map (  line => append_with_after_match (line) )
        .map (  line => replace_case (line) )
    )

  def append_with_after_match (line : String) : String =
    if ( line.trim ().startsWith (soda_match_pattern)
    ) line + tc.coq_space + tc.coq_with_reserved_word
    else line

  def replace_case (line : String) : String =
    if ( line.trim.startsWith (soda_case_pattern)
    ) ReplacementAux_ (). replace_first (line) (soda_case_pattern) (tc.coq_case_translation)
    else line

}

case class MatchCaseBlockTranslator_ () extends MatchCaseBlockTranslator
