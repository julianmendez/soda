package soda.translator.replacement

trait AnnotatedLine {

  def line: String

  def isComment: Boolean

}

case class AnnotatedLine_ (line: String, isComment: Boolean )  extends AnnotatedLine

/**
 * This preprocessor annotates lines to determine whether they are comments.
 */
trait CommentPreprocessor  extends soda.translator.block.MultiLineProcessor {

  import soda.lib.Recursion_

  lazy val soda_begin_comment = "/*"

  lazy val soda_end_comment = "*/"

  lazy val annotated_lines: Seq [AnnotatedLine] =
    Recursion_ () .fold (lines, initial_value, next_value_function )
      .annotated_lines_rev
      .reverse

  lazy val initial_value = PreprocessorFoldTuple_ (false, Seq ()  )

  def next_value_function (pair: PreprocessorFoldTuple, line: String ): PreprocessorFoldTuple =
    {
      lazy val t = annotate_this_line (line, pair.comment_state )
      PreprocessorFoldTuple_ (t.new_comment_state, pair.annotated_lines_rev.+: (AnnotatedLine_ (line, t.current_state )  )  ) }

  def annotate_this_line (line: String, comment_state: Boolean ): CurrentAndNewCommentState =
    if (comment_state
    ) CurrentAndNewCommentState_ (true, ! line.trim.endsWith (soda_end_comment )  )
    else
      if (line.trim.startsWith (soda_begin_comment )
      ) CurrentAndNewCommentState_ (true, ! line.trim.endsWith (soda_end_comment )  )
      else CurrentAndNewCommentState_ (false, false )

}

case class CommentPreprocessor_ (lines: Seq [String]  )  extends CommentPreprocessor

trait PreprocessorFoldTuple {

  def comment_state: Boolean

  def annotated_lines_rev: Seq [AnnotatedLine]

}

case class PreprocessorFoldTuple_ (comment_state: Boolean, annotated_lines_rev: Seq [AnnotatedLine]  )  extends PreprocessorFoldTuple

trait CurrentAndNewCommentState {

  def current_state: Boolean

  def new_comment_state: Boolean

}

case class CurrentAndNewCommentState_ (current_state: Boolean, new_comment_state: Boolean )  extends CurrentAndNewCommentState
