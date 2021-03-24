package scopus.translator

import scopus.lib.Rec


case class Replacement (  ) {

  lazy val ScopusSpace: String = " "
  lazy val ScalaSpace: String = " "

  def replace_all ( line: String , pattern: String , replacement: String ) : String = {
    lazy val result = rec ( line , pattern , replacement , Seq (  )  )

    import scala.annotation.tailrec
        @tailrec
    def rec ( line: String , pattern: String , replacement: String , replaced_text_rev: Seq [ String ]  ) : String = {
      lazy val pos = line.indexOf ( pattern )
      if ( pos == -1
      )
        replaced_text_rev.+: ( line )
          .reverse
          .mkString ("")
      else {
        lazy val new_replaced_text_rev = replaced_text_rev.+: ( line.substring ( 0 , pos ) + replacement )
        lazy val new_line = line.substring ( pos + pattern.length )
        rec ( new_line , pattern , replacement , new_replaced_text_rev )
      }
    }

    result
  }

  def replace_at_beginning ( line: String , index: Int , translator: Translator ) : Some [ String ] =
    if ( index == 0
    ) replace ( line , translator , only_beginning=true )
    else Some ( line )


  def replace ( line: String , translator: Translator , only_beginning: Boolean ) : Some [ String ] = {
    lazy val result = Some ( Rec (  ) .foldLeft ( translator.keys , initval , op )  )

    def replace_if_found ( line: String , pattern: String , newText: String , only_beginning: Boolean ) : String =
      if ( ( only_beginning && line.trim.startsWith ( pattern.trim )  ) ||
        ( ! only_beginning && line.contains ( pattern )  )
      ) replace_all ( line , pattern , newText )
      else line

    lazy val initval: String = line

    def op ( line: String , reserved_word: String ) : String =
      replace_if_found ( line ,        ScopusSpace + reserved_word + ScopusSpace , ScalaSpace + translator.translate ( reserved_word ) + ScalaSpace , only_beginning )

    result
  }

  def add_space_to_scopus_line ( line: String ) : String = ScopusSpace + line + ScopusSpace

  def add_spaces_to_symbols ( line: String , symbols: Set [ Char ]  ) : Some [ String ] = Some (
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
  )


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

  def add_after_spaces ( text_to_prepend: String , line: String ) : String = {
    lazy val prefix_length = line.takeWhile ( ch => ch.isSpaceChar ) .length
    line.substring ( 0 , prefix_length ) + text_to_prepend + line.substring ( prefix_length )
  }

}
