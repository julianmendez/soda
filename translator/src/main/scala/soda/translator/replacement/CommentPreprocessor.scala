package soda.translator.replacement


case class AnnotatedLine (line: String, isComment: Boolean )

/**
 * This preprocessor annotates lines to determine whether they are comments.
 */
trait CommentPreprocessor {
  import soda.lib.Rec

  def lines: Seq [String]

  lazy val soda_begin_comment = "/*"

  lazy val soda_end_comment = "*/"

  lazy val annotated_lines: Seq [AnnotatedLine] =
    Rec () .fold (lines, initial_value, next_value_function )
      .annotated_lines_rev
      .reverse

  lazy val initial_value = PreprocessorFoldTuple (false, Seq ()  )

  def next_value_function (pair: PreprocessorFoldTuple, line: String ): PreprocessorFoldTuple =
    {
      lazy val t = annotate_this_line (line, pair.comment_state )
      PreprocessorFoldTuple (t.new_comment_state, pair.annotated_lines_rev.+: (AnnotatedLine (line, t.current_state )  )  ) }

  def annotate_this_line (line: String, comment_state: Boolean ): CurrentAndNewCommentState =
    if (comment_state
    ) CurrentAndNewCommentState (true, ! line.trim.endsWith (soda_end_comment )  )
    else
      if (line.trim.startsWith (soda_begin_comment )
      ) CurrentAndNewCommentState (true, ! line.trim.endsWith (soda_end_comment )  )
      else CurrentAndNewCommentState (false, false )
}

case class CommentPreprocessorImpl (lines: Seq [String]  ) extends CommentPreprocessor

case class PreprocessorFoldTuple (comment_state: Boolean, annotated_lines_rev: Seq [AnnotatedLine]  )

case class CurrentAndNewCommentState (current_state: Boolean, new_comment_state: Boolean )
