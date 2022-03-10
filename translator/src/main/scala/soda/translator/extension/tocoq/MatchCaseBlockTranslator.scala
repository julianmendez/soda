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

  private lazy val _sc = SodaConstant_ ()

  private lazy val _soda_case_pattern = _sc.case_reserved_word + _sc.space

  private lazy val _tc = TranslationConstantToCoq_ ()

  private lazy val _soda_match_pattern = _sc.match_reserved_word + " "

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
        .map (  line => _append_with_after_match (line) )
        .map (  line => _replace_case (line) )
    )

  private def _append_with_after_match (line : String) : String =
    if ( line.trim ().startsWith (_soda_match_pattern)
    ) line + _tc.coq_space + _tc.coq_with_reserved_word
    else line

  private def _replace_case (line : String) : String =
    if ( line.trim.startsWith (_soda_case_pattern)
    ) ReplacementAux_ (). replace_first (line) (_soda_case_pattern) (_tc.coq_case_translation)
    else line

}

case class MatchCaseBlockTranslator_ () extends MatchCaseBlockTranslator
