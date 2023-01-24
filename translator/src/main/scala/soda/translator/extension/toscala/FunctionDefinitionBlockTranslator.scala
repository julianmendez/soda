package soda.translator.extension.toscala

trait FunctionDefinitionBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.Block
  import   soda.translator.block.Block_
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_
  import   soda.translator.replacement.Replacement_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case FunctionDefinitionAnnotation_ (block) => _translate_function_definition_block (block)
      case x => annotated_block
    }

  private def _translate_function_definition_block (block : Block) : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation_ (_translate_block (block) )

  private def _translate_block (block : Block) : Block =
     if ( block.readable_lines.isEmpty
     ) block
     else _translate_block_with (block.readable_lines.head) (block)

  private def _translate_block_with (first_line : AnnotatedLine) (block : Block) : Block =
    if ( _is_annotation (first_line.line)
    ) _prepend_line (first_line.line) (_translate_main_block (_remove_first_line_if_possible (block) ) )
    else _translate_main_block (block)

  private def _translate_main_block (block : Block) : Block =
    _translate_main_block_with (block) ( FunctionDefinitionLineDetector_ (_flatten_block (block) ) )

  private def _translate_main_block_with (block : Block) (detector : FunctionDefinitionLineDetector) : Block =
    detector.detect match  {
      case detector.val_detected => _replace_on_val_block (_get_initial_comment (block.annotated_lines) ) (_get_part_without_initial_comment (block.annotated_lines) )
      case detector.def_detected => _replace_on_def_block (_get_initial_comment (block.annotated_lines) ) (_get_part_without_initial_comment (block.annotated_lines) )
      case x => block
    }

  private def _replace_on_val_block (initial_comments : Seq [AnnotatedLine] ) (main_block : Seq [AnnotatedLine] ) : Block =
    Block_ (
      initial_comments .++ (_replace_first_line (main_block) (_translate_val_definition (main_block.head.line) ) )
    )

  private def _replace_on_def_block (initial_comments : Seq [AnnotatedLine] ) (main_block : Seq [AnnotatedLine] ) : Block =
    Block_ (
      initial_comments .++ (_replace_first_line (main_block) (_translate_def_definition (main_block.head.line) ) )
    )

  private def _get_initial_comment (lines : Seq [AnnotatedLine] ) : Seq [AnnotatedLine] =
    lines.takeWhile (  annotated_line => annotated_line.is_comment )

  private def _get_part_without_initial_comment (lines : Seq [AnnotatedLine] ) : Seq [AnnotatedLine] =
    lines.dropWhile (  annotated_line => annotated_line.is_comment )

  private def _translate_val_definition (line : String) : String =
    Replacement_ (line)
      .add_after_spaces_or_pattern (_tc.scala_space) (_private_prefix_if_necessary (line) + _tc.scala_value + _tc.scala_space)
      .line

  private def _translate_def_definition (line : String) : String =
    Replacement_ (line)
      .add_after_spaces_or_pattern (_tc.scala_space) (_private_prefix_if_necessary (line) + _tc.scala_definition + _tc.scala_space)
      .line

  private def _private_prefix_if_necessary (line : String) : String =
    if ( line.trim.startsWith (_sc.private_function_prefix)
    ) _tc.scala_private_reserved_word + _tc.scala_space
    else ""

  private def _replace_first_line (lines : Seq [AnnotatedLine] ) (new_first_line : String) : Seq [AnnotatedLine] =
    if ( lines.isEmpty
    ) Seq [AnnotatedLine] () .+: ( AnnotatedLine_ (new_first_line, false) )
    else lines.tail .+: ( AnnotatedLine_ (new_first_line, false) )

  private def _remove_first_line_if_possible (block : Block) : Block =
    if ( block.lines.isEmpty
    ) block
    else BlockBuilder_ ().build (block.lines.tail)

  private def _prepend_line (line : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      Seq [String] (line) .++ (block.lines)
    )

  private def _flatten_block (block : Block) : String =
    block.lines.mkString (_sc.space)

  private def _is_annotation (line : String) : Boolean =
    (line.trim == _sc.tail_recursion_annotation) || (line.trim == _sc.override_annotation)

}

case class FunctionDefinitionBlockTranslator_ () extends FunctionDefinitionBlockTranslator
