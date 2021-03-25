package scopus.translator.replacement

import scopus.lib.Rec


/**
 * A token is a piece of code, that can contain one or more words combined with symbols.
 */
case class Token ( text: String , parser_state: ParserState , index: Int ) {

  override
  lazy val toString: String =
    "(\"" + text + "\"," + parser_state + ", " + index + ")"
}

/**
 * This class processes a line to divide it into tokens.
 */
case class Tokenizer (  ) {

  def tokenize ( line: String ) : Seq [ Token ] = {
    lazy val result = postproc ( Rec (  ) .foldLeft ( Range ( 0 , line.length )  , initval , op )  )

    case class RecTuple ( last_index: Int , parser_state: ParserState , rev_tokens: Seq [ Token ]  )

    lazy val initval = RecTuple ( 0 , ParserStateEnum (  ) .Plain , Seq (  )  )

    def postproc ( tuple: RecTuple ) : Seq [ Token ] =
      ( tuple.rev_tokens.+: ( Token ( line.substring ( tuple.last_index )  , tuple.parser_state , tuple.last_index )  )  )
        .reverse

    def op ( tuple: RecTuple , current_index: Int ) : RecTuple = {
      lazy val ch = line.charAt ( current_index )
      lazy val char_type = CharTypeEnum (  ) .get_char_type ( ch )
      lazy val new_parser_state = ParserTransition (  ) .next_parser_state ( tuple.parser_state , char_type )

      if ( ParserStateEnum (  ) .is_same_class ( new_parser_state , tuple.parser_state )
      ) RecTuple ( tuple.last_index , new_parser_state , tuple.rev_tokens )
      else {
        lazy val index =
          if ( tuple.parser_state == ParserStateEnum (  ) .QuotesState ||
             tuple.parser_state == ParserStateEnum (  ) .ApostropheState
          ) current_index + 1
          else current_index

        lazy val text = line.substring ( tuple.last_index , index )
        RecTuple ( index , new_parser_state ,          tuple.rev_tokens.+: ( Token ( text , tuple.parser_state , tuple.last_index )  )  )
      }
    }

    result
  }

}
