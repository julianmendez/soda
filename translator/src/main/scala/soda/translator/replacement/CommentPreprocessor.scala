package soda.translator.replacement


case class AnnotatedLine (line: String, isComment: Boolean )

/**
 * This preprocessor annotates lines to determine whether they are comments.
 */
trait CommentPreprocessor {
  import soda.lib.Rec

  def lines: Seq [String]

  lazy val SodaBeginComment = "/*"
  lazy val SodaEndComment = "*/"

  lazy val get_annotated_lines: Seq [AnnotatedLine] =
    Rec () .foldLeft (lines, initial_value, next_value )
      .annotated_lines_rev
      .reverse

  lazy val initial_value = PreprocessorFoldTuple (false, Seq ()  )

  def next_value (pair: PreprocessorFoldTuple, line: String ): PreprocessorFoldTuple =
    {
      lazy val t = annotate_this_line (line, pair.comment_state )
      PreprocessorFoldTuple (t.new_comment_state, pair.annotated_lines_rev.+: (AnnotatedLine (line, t.current_state )  )  ) }

  def annotate_this_line (line: String, comment_state: Boolean ): CurrentAndNewCommentState =
    if (comment_state
    ) CurrentAndNewCommentState (true, ! line.trim.endsWith (SodaEndComment )  )
    else
      if (line.trim.startsWith (SodaBeginComment )
      ) CurrentAndNewCommentState (true, ! line.trim.endsWith (SodaEndComment )  )
      else CurrentAndNewCommentState (false, false )
}

case class CommentPreprocessorImpl (lines: Seq [String]  ) extends CommentPreprocessor

case class PreprocessorFoldTuple (comment_state: Boolean, annotated_lines_rev: Seq [AnnotatedLine]  )

case class CurrentAndNewCommentState (current_state: Boolean, new_comment_state: Boolean )
