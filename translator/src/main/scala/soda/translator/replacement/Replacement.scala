package scopus.translator.replacement

import scopus.lib.Rec

/**
 * This models a collection of replacement functions.
 * This is intended to be used as a pipeline.
 */
case class Replacement ( line: String ) {

  lazy val ScopusSpace: String = " "
  lazy val ScalaSpace: String = " "

  def replace_with ( function: String => String ) : Replacement =
    Replacement ( function ( line )  )

  def replace_at_beginning ( index: Int , translator: Translator ) : Replacement =
    Replacement ( replace_at_beginning ( line , index , translator )  )

  def replace_at_beginning ( line: String , index: Int , translator: Translator ) : String =
    if ( index == 0
    ) replace ( line , translator , only_beginning=true )
    else line

  def replace ( translator: Translator , only_beginning: Boolean ) : Replacement =
    Replacement ( replace ( line , translator , only_beginning )  )

  def replace ( line: String , translator: Translator , only_beginning: Boolean ) : String = {
    lazy val result = Rec (  ) .foldLeft ( translator.keys , initial_value , next_value )

    def replace_if_found ( line: String , pattern: String , newText: String , only_beginning: Boolean ) : String =
      if ( ( only_beginning && line.trim.startsWith ( pattern.trim )  ) ||
        ( ! only_beginning && line.contains ( pattern )  )
      ) replace_all ( line , pattern , newText )
      else line

    lazy val initial_value: String = line

    def next_value ( line: String , reserved_word: String ) : String =
      replace_if_found ( line ,        ScopusSpace + reserved_word + ScopusSpace , ScalaSpace + translator.translate ( reserved_word ) + ScalaSpace , only_beginning )

    result
  }

  def replace_all ( line: String , pattern: String , replacement: String ) : String = {
    lazy val result = postproc (
      Rec (  ) .foldLeftWhile ( Range ( 0 , line.length )  , initial_value , next_value , condition )
    )

    lazy val initial_value = FoldTuple ( Seq (  )  , 0 )

    def next_value ( tuple: FoldTuple , x: Int ) : FoldTuple = {
      lazy val replaced_text_rev = tuple.replaced_text_rev
      lazy val start_index = tuple.start_index
      lazy val pos = line.indexOf ( pattern , start_index )
      lazy val next_tuple =
        if ( pos == -1
        ) FoldTuple ( replaced_text_rev.+: ( line.substring ( start_index )  )  , pos )
        else {
          lazy val new_replaced_text_rev = ( replaced_text_rev.+: ( line.substring ( start_index , pos )  )  ) .+: ( replacement )
          lazy val new_index = pos + pattern.length
          FoldTuple ( new_replaced_text_rev , new_index )
        }
      next_tuple
    }

    def condition ( tuple: FoldTuple , x: Int ) : Boolean =
      ! ( tuple.start_index == -1 )

    def postproc ( tuple: FoldTuple ) : String =
      tuple.replaced_text_rev.reverse.mkString ("")

    case class FoldTuple ( replaced_text_rev: Seq [ String ]  , start_index: Int )

    result
  }

  def add_space_to_scopus_line (  ) : Replacement =
    Replacement ( ScopusSpace + line + ScopusSpace )

  def add_spaces_to_symbols ( symbols: Set [ Char ]  ) : Replacement =
    Replacement ( add_spaces_to_symbols ( line , symbols )  )

  def add_spaces_to_symbols ( line: String , symbols: Set [ Char ]  ) : String =
    line.indices.map ( index => {
      lazy val result = left_part + ch + right_part

      lazy val ch = line ( index )

      lazy val left_part =
        if ( ( index > 0 ) &&
          symbols.contains ( ch ) &&
          ! line ( index - 1 ) .isWhitespace
        ) ScalaSpace
        else ""

      lazy val right_part =
        if ( ( index < line.length - 1 ) &&
          symbols.contains ( ch ) &&
          ! line ( index + 1 ) .isWhitespace
        ) ScalaSpace
        else ""

      result
    } ) .mkString ("")

  def remove_space_from_scala_line (  ) : Replacement =
    Replacement ( remove_space_from_scala_line ( line )  )

  def remove_space_from_scala_line ( line: String ) : String = {
    lazy val line_without_starting_space =
      if ( line.startsWith ( ScalaSpace )
      ) line.substring ( 1 )
      else line

    lazy val line_without_ending_space =
      if ( line_without_starting_space.endsWith ( ScalaSpace )
      ) line_without_starting_space.substring ( 0 , line_without_starting_space.length - 1 )
      else line_without_starting_space

    line_without_ending_space
  }

  def add_after_spaces ( text_to_prepend: String ) : Replacement =
    Replacement ( add_after_spaces ( line , text_to_prepend )  )

  def add_after_spaces ( line: String , text_to_prepend: String ) : String = {
    lazy val prefix_length = line.takeWhile ( ch => ch.isSpaceChar ) .length
    line.substring ( 0 , prefix_length ) + text_to_prepend + line.substring ( prefix_length )
  }

}