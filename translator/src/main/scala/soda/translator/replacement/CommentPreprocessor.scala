package soda.translator.replacement


case class AnnotatedLine (line: String, isComment: Boolean )

/**
 * This preprocessor annotates lines to determine whether they are comments.
 */
case class CommentPreprocessor (lines: Seq [String]  ) {
  import soda.lib.Rec

  lazy val SodaBeginComment = "/*"
  lazy val SodaEndComment = "*/"

  lazy val get_annotated_lines: Seq [AnnotatedLine] =
    Rec () .foldLeft (lines, initial_value, next_value )
      .annotated_lines_rev
      .reverse

  case class FoldTuple (comment_state: Boolean, annotated_lines_rev: Seq [AnnotatedLine]  )

  lazy val initial_value = FoldTuple (false, Seq ()  )

  def next_value (pair: FoldTuple, line: String ): FoldTuple = {
    lazy val t = annotate_this_line (line, pair.comment_state )
    FoldTuple (t.new_comment_state, pair.annotated_lines_rev.+: (AnnotatedLine (line, t.current_state )  )  )
  }

  case class CurrentAndNewCommentState (current_state: Boolean, new_comment_state: Boolean )

  def annotate_this_line (line: String, comment_state: Boolean ): CurrentAndNewCommentState =
    if (comment_state
    ) CurrentAndNewCommentState (true, ! line.trim.endsWith (SodaEndComment )  )
    else
      if (line.trim.startsWith (SodaBeginComment )
      ) CurrentAndNewCommentState (true, ! line.trim.endsWith (SodaEndComment )  )
      else CurrentAndNewCommentState (false, false )
}
