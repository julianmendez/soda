package soda.translator.replacement


/**
 * This models a collection of replacement functions.
 * This is intended to be used as a pipeline.
 */
case class Replacement (line: String ) {
  import soda.lib.Rec

  lazy val SodaSpace: String = " "
  lazy val ScalaSpace: String = " "

  def replace_with (function: String => String ): Replacement =
    Replacement (function (line )  )

  def replace_at_beginning (index: Int, translator: Translator ): Replacement =
    Replacement (replace_at_beginning (line, index, translator )  )

  def replace_at_beginning (line: String, index: Int, translator: Translator ): String =
    if (index == 0
    ) replace_only_beginning (line, translator )
    else line

  def replace_only_beginning (line: String, translator: Translator ): String = {
    lazy val result = Rec () .foldLeft (translator.keys, initial_value, next_value )

    lazy val initial_value: String = line

    def next_value (line: String, reserved_word: String ): String =
      _replace_if_found (line, SodaSpace + reserved_word + SodaSpace, ScalaSpace + translator.translate (reserved_word ) + ScalaSpace )

    result
  }

  def _replace_if_found (line: String, pattern: String, new_text: String ): String =
    if (line.trim.startsWith (pattern.trim )
    ) replace_all (line, pattern, new_text )
    else line

  def replace_all (pattern: String, replacement: String ): Replacement =
    Replacement (replace_all (line, pattern, replacement )  )

  def replace_all (line: String, pattern: String, replacement: String ): String =
    Replacer (line, pattern, replacement ) .replace

  case class Replacer (line: String, pattern: String, replacement: String ) {
    lazy val replace =
      postproc (Rec () .foldLeftWhile (Rec () .range (line.length ), initial_value, next_value, condition ) )

    lazy val initial_value = FoldTuple (Seq (), 0 )

    def next_value (tuple: FoldTuple, x: Int ): FoldTuple = {
      lazy val replaced_text_rev = tuple.replaced_text_rev
      lazy val start_index = tuple.start_index
      lazy val pos = line.indexOf (pattern, start_index )
      lazy val next_tuple =
        if (pos == -1
        ) FoldTuple (replaced_text_rev.+: (line.substring (start_index )  ), pos )
        else {
          lazy val new_replaced_text_rev = (replaced_text_rev.+: (line.substring (start_index, pos )  )  ) .+: (replacement )
          lazy val new_index = pos + pattern.length
          FoldTuple (new_replaced_text_rev, new_index )
        }
      next_tuple
    }

    def condition (tuple: FoldTuple, x: Int ): Boolean =
      ! (tuple.start_index == -1 )

    def postproc (tuple: FoldTuple ): String =
      tuple.replaced_text_rev.reverse.mkString ("")

    case class FoldTuple (replaced_text_rev: Seq [String], start_index: Int )
  }

  def replace (translator: Translator ): Replacement =
    Replacement (replace (line, translator )  )

  def replace (line: String, translator: Translator ): String = {
    lazy val result = Rec () .foldLeft (translator.keys, initial_value, next_value )

    lazy val initial_value: String = line

    def next_value (line: String, reserved_word: String ): String =
      replace_if_found (line, SodaSpace + reserved_word + SodaSpace, ScalaSpace + translator.translate (reserved_word ) + ScalaSpace )

    def replace_if_found (line: String, pattern: String, new_text: String ): String =
      if (line.contains (pattern )
      ) replace_all (line, pattern, new_text )
      else line

    result
  }

  def add_space_to_soda_line (): Replacement =
    Replacement (SodaSpace + line + SodaSpace )

  def add_spaces_to_symbols (symbols: Set [Char]  ): Replacement =
    Replacement (add_spaces_to_symbols (line, symbols )  )

  def add_spaces_to_symbols (line: String, symbols: Set [Char]  ): String =
    line.indices.map (index => {
      lazy val result = left_part + ch + right_part

      lazy val ch = line (index )

      lazy val left_part =
        if ((index > 0 ) &&
          symbols.contains (ch ) &&
          ! line (index - 1 ) .isWhitespace
        ) ScalaSpace
        else ""

      lazy val right_part =
        if ((index < line.length - 1 ) &&
          symbols.contains (ch ) &&
          ! line (index + 1 ) .isWhitespace
        ) ScalaSpace
        else ""

      result
    } ) .mkString ("")

  def remove_space_from_scala_line (): Replacement =
    Replacement (remove_space_from_scala_line (line )  )

  def remove_space_from_scala_line (line: String ): String = {
    lazy val line_without_starting_space =
      if (line.startsWith (ScalaSpace )
      ) line.substring (1 )
      else line

    lazy val line_without_ending_space =
      if (line_without_starting_space.endsWith (ScalaSpace )
      ) line_without_starting_space.substring (0, line_without_starting_space.length - 1 )
      else line_without_starting_space

    line_without_ending_space
  }

  def add_after_spaces (text_to_prepend: String ): Replacement =
    Replacement (add_after_spaces (line, text_to_prepend )  )

  def add_after_spaces (line: String, text_to_prepend: String ): String = {
    lazy val prefix_length = line.takeWhile (ch => ch.isSpaceChar ) .length
    line.substring (0, prefix_length ) + text_to_prepend + line.substring (prefix_length )
  }

  def replace_regex (translator: Translator ): Replacement =
    Replacement (replace_regex (line, translator )  )

  def replace_regex (line: String, translator: Translator ): String = {
    lazy val result = Rec () .foldLeft (translator.keys, initial_value, next_value )

    lazy val initial_value: String = line

    def next_value (line: String, regex: String ): String =
      line.replaceAll (regex, translator.translate (regex )  )

    result
  }

}
