package soda.translator.extension.toscala

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

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  private lazy val _soda_match_pattern = _sc.match_reserved_word + " "

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
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
    BlockBuilder_ ().build (
      block.lines
        .map (  line => _insert_match_before_brace_if_found (line) )
        .map (  line => _replace_match_end_if_found (line) )
    )

  private def _insert_match_before_brace_if_found (line : String) : String =
    if ( line.trim.startsWith (_soda_match_pattern)
    ) _assemble_parts (index = line.indexOf (_soda_match_pattern) ) (line)
    else line

  private def _replace_match_end_if_found (line : String) : String =
    if ( line.trim == _sc.match_end_reserved_word
    ) line.replaceAll (_sc.match_end_reserved_word, _tc.scala_match_end_translation)
    else line

  private def _assemble_parts (index : Int) (line : String) : String =
    (_left_part (index) (line) ) + (_right_part (index) (line) ) + _tc.scala_match_translation + _tc.scala_space + _tc.scala_opening_brace

  private def _left_part (index : Int) (line : String) : String =
    line.substring (0, index)

  private def _right_part (index : Int) (line : String) : String =
    line.substring (index + _soda_match_pattern.length, line.length)

}

case class MatchCaseBlockTranslator_ () extends MatchCaseBlockTranslator
