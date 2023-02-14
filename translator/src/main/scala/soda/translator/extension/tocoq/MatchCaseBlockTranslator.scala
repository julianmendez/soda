package soda.translator.extension.tocoq

/*
 * This package contains classes for the translation to Gallina, the specification language used by Coq.
 */





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
    if ( _is_a_match_case_structure (block)
    ) _translate_match_case_structure (block) (_get_tabulation_of_match (block) )
    else block

  private def _is_a_match_case_structure (block : AnnotatedBlock) : Boolean =
    block.lines.exists (  line => _is_a_match_line (line) )

  private def _is_a_match_line (line : String) : Boolean =
    line.trim.startsWith (_soda_match_pattern)

  private def _is_a_case_line (line : String) : Boolean =
    line.trim.startsWith (_soda_case_pattern)

  private def _get_tabulation_of_match (block : AnnotatedBlock) : String =
    block.lines
      .find (  line => _is_a_match_line (line) )
      .map (  line => _left_part (line.indexOf (_soda_match_pattern) ) (line) )
      .getOrElse (_tc.coq_space)

  private def _translate_match_case_structure (block: AnnotatedBlock) (tabulation : String) : Block =
    BlockBuilder_ ().build (
      block.lines
        .map (  line => _append_with_after_match (line) )
        .map (  line => _replace_case (line) )
         .++ ( Seq [String] () .+: (tabulation + _tc.coq_match_end_translation) )
    )

  private def _append_with_after_match (line : String) : String =
    if ( _is_a_match_line (line)
    ) line + _tc.coq_space + _tc.coq_with_reserved_word
    else line

  private def _replace_case (line : String) : String =
    if ( _is_a_case_line (line)
    ) ReplacementAux_ (). replace_first (line) (_soda_case_pattern) (_tc.coq_case_translation)
    else line

  private def _left_part (index : Int) (line : String) : String =
    line.substring (0, index)

}

case class MatchCaseBlockTranslator_ () extends MatchCaseBlockTranslator
