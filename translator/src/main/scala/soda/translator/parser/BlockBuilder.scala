package soda.translator.parser

trait BlockBuilder
{

  import   soda.lib.Recursion_
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedBlock_
  import   soda.translator.block.Block
  import   soda.translator.block.Block_

  private lazy val _sc = SodaConstant_ ()

  def build (lines : Seq [String] ) : Block =
    Block_ (
      _get_annotated_lines (lines)
    )

  private def _get_annotated_lines (lines : Seq [String] ) : Seq [AnnotatedLine] =
    Recursion_ ().fold (lines) (_get_annotated_lines_initial_value) (_get_annotated_lines_next_value_function)
      .annotated_lines_rev
      .reverse

  private lazy val _get_annotated_lines_initial_value  : PreprocessorFoldTuple = PreprocessorFoldTuple_ (false, Seq () )

  private def _get_annotated_lines_next_value_function (pair : PreprocessorFoldTuple) (line : String) : PreprocessorFoldTuple =
    _get_annotated_lines_next_value_function_with (_annotate_this_line (line) (pair.comment_state) ) (pair) (line)

  private def _get_annotated_lines_next_value_function_with (t : CurrentAndNewCommentState) (pair : PreprocessorFoldTuple) (line : String) : PreprocessorFoldTuple =
    PreprocessorFoldTuple_ (t.new_comment_state, pair.annotated_lines_rev.+: (AnnotatedLine_ (line, t.current_state) ) )

  private def _annotate_this_line (line : String) (comment_state : Boolean) : CurrentAndNewCommentState =
    if ( comment_state
    ) CurrentAndNewCommentState_ (true, ! line.trim.endsWith (_sc.comment_closing_symbol) )
    else
      if ( line.trim.startsWith (_sc.comment_opening_symbol)
      ) CurrentAndNewCommentState_ (true, ! line.trim.endsWith (_sc.comment_closing_symbol) )
      else CurrentAndNewCommentState_ (false, false)

}

case class BlockBuilder_ () extends BlockBuilder

trait PreprocessorFoldTuple
{

  def   comment_state : Boolean
  def   annotated_lines_rev : Seq [soda.translator.block.AnnotatedLine]

}

case class PreprocessorFoldTuple_ (comment_state : Boolean, annotated_lines_rev : Seq [soda.translator.block.AnnotatedLine]) extends PreprocessorFoldTuple

trait CurrentAndNewCommentState
{

  def   current_state : Boolean
  def   new_comment_state : Boolean

}

case class CurrentAndNewCommentState_ (current_state : Boolean, new_comment_state : Boolean) extends CurrentAndNewCommentState
