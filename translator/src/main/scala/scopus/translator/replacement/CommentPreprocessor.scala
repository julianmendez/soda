package scopus.translator.replacement

import scopus.lib.Rec

case class AnnotatedLine ( line: String , isComment: Boolean )

/**
 * This preprocessor annotates lines to determine whether they are comments.
 */
case class CommentPreprocessor (  ) {

  lazy val ScopusBeginComment = "/*"
  lazy val ScopusEndComment = "*/"

  def annotate_lines ( lines: Seq [ String ]  ) : Seq [ AnnotatedLine ] = {
    lazy val result =
      Rec (  ) .foldLeft ( lines , initval , op )
        .annotated_lines_rev
        .reverse

    case class RecPair ( comment_state: Boolean , annotated_lines_rev: Seq [ AnnotatedLine ]  )

    lazy val initval = RecPair ( false , Seq (  )  )

    def op ( pair: RecPair , line: String ) : RecPair = {
      lazy val ( current_state , new_comment_state ) = annotate_this_line ( line , pair.comment_state )
      RecPair ( new_comment_state , pair.annotated_lines_rev.+: ( AnnotatedLine ( line , current_state )  )  )
    }

    def annotate_this_line ( line: String , comment_state: Boolean ) : ( Boolean , Boolean ) =
      if ( comment_state
      ) ( true , ! line.trim.endsWith ( ScopusEndComment )  )
      else
        if ( line.trim.startsWith ( ScopusBeginComment )
        ) ( true , ! line.trim.endsWith ( ScopusEndComment )  )
        else ( false , false )

    result
  }

}
