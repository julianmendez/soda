package scopus.translator.replacement


case class AnnotatedLine ( line: String , isComment: Boolean )

/**
 * This preprocessor annotates lines to determine whether they are comments.
 */
case class CommentPreprocessor (  ) {

  lazy val ScopusBeginComment = "/*"
  lazy val ScopusEndComment = "*/"

  def annotate_lines ( lines: Seq [ String ]  ) : Seq [ AnnotatedLine ] = {
    lazy val result = rec ( lines , comment_state=false , Seq (  )  )

    import scala.annotation.tailrec
        @tailrec
    def rec ( lines: Seq [ String ]  , comment_state: Boolean , annotated_lines_rev: Seq [ AnnotatedLine ]  ) : Seq [ AnnotatedLine ] =
      if ( lines.isEmpty
      ) annotated_lines_rev.reverse
      else {
        lazy val line = lines.head
        lazy val ( current_state , new_comment_state ) =
          if ( comment_state
          ) ( true , ! line.trim.endsWith ( ScopusEndComment )  )
          else
            if ( line.trim.startsWith ( ScopusBeginComment )
            ) ( true , ! line.trim.endsWith ( ScopusEndComment )  )
            else ( false , false )

        rec ( lines.tail , new_comment_state , annotated_lines_rev.+: ( AnnotatedLine ( line , current_state )  )  )
      }

    result
  }

}
