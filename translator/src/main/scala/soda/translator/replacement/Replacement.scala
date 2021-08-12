package soda.translator.replacement


/**
 * This models a collection of replacement functions.
 * This is intended to be used as a pipeline.
 */
trait Replacement {
  import soda.lib.Rec

  def line: String

  lazy val soda_space: String = " "

  lazy val scala_space: String = " "

  def replace_with (function: String => String ): Replacement =
    Replacement_ (function (line )  )

  def replace_at_beginning (index: Int, translator: Translator ): Replacement =
    Replacement_ (replace_at_beginning (line, index, translator )  )

  def replace_at_beginning (line: String, index: Int, translator: Translator ): String =
    if (index == 0
    ) replace_only_beginning (line, translator )
    else line

  def replace_only_beginning (line: String, translator: Translator ): String =
    {
      lazy val initial_value: String = line

      def next_value_function (line: String, reserved_word: String ): String =
        _replace_if_found (line, soda_space + reserved_word + soda_space, scala_space + translator.translate (reserved_word ) + scala_space )

      Rec () .fold (translator.keys, initial_value, next_value_function ) }

  def _replace_if_found (line: String, pattern: String, new_text: String ): String =
    if (line.trim.startsWith (pattern.trim )
    ) replace_all (line, pattern, new_text )
    else line

  def replace_all (pattern: String, replacement: String ): Replacement =
    Replacement_ (replace_all (line, pattern, replacement )  )

  def replace (translator: Translator ): Replacement =
    Replacement_ (replace (line, translator )  )

  def replace (line: String, translator: Translator ): String =
    {
      lazy val initial_value: String = line

      def next_value_function (line: String, reserved_word: String ): String =
        replace_if_found (line, soda_space + reserved_word + soda_space, scala_space + translator.translate (reserved_word ) + scala_space )

      Rec () .fold (translator.keys, initial_value, next_value_function ) }

  def replace_if_found (line: String, pattern: String, new_text: String ): String =
    if (line.contains (pattern )
    ) replace_all (line, pattern, new_text )
    else line

  def replace_all (line: String, pattern: String, replacement: String ): String =
    Replacer_ (line, pattern, replacement ) .replaced_text

  def add_space_to_soda_line (): Replacement =
    Replacement_ (soda_space + line + soda_space )

  def add_spaces_to_symbols (symbols: Set [Char]  ): Replacement =
    Replacement_ (add_spaces_to_symbols (line, symbols )  )

  def add_spaces_to_symbols (line: String, symbols: Set [Char]  ): String =
    line.indices.map (index =>
      {
        lazy val ch = line (index )
        lazy val left_part =
          if ((index > 0 ) && symbols.contains (ch ) &&
            ! line (index - 1 ) .isWhitespace
          ) scala_space
          else ""
        lazy val right_part =
          if ((index < line.length - 1 ) && symbols.contains (ch ) &&
            ! line (index + 1 ) .isWhitespace
          ) scala_space
          else ""

        left_part + ch + right_part }    ) .mkString ("")

  def remove_space_from_scala_line (): Replacement =
    Replacement_ (remove_space_from_scala_line (line )  )

  def remove_space_from_scala_line (line: String ): String =
    {
      lazy val line_without_starting_space =
        if (line.startsWith (scala_space )
        ) line.substring (1 )
        else line
      lazy val line_without_ending_space =
        if (line_without_starting_space.endsWith (scala_space )
        ) line_without_starting_space.substring (0, line_without_starting_space.length - 1 )
        else line_without_starting_space
      line_without_ending_space }

  def add_after_spaces (text_to_prepend: String ): Replacement =
    Replacement_ (add_after_spaces (line, text_to_prepend )  )

  def add_after_spaces (line: String, text_to_prepend: String ): String =
    {
      lazy val prefix_length = line.takeWhile (ch => ch.isSpaceChar ) .length
      line.substring (0, prefix_length ) + text_to_prepend + line.substring (prefix_length ) }

  def replace_regex (translator: Translator ): Replacement =
    Replacement_ (replace_regex (line, translator )  )

  def replace_regex (line: String, translator: Translator ): String =
    {
      lazy val initial_value: String = line
      def next_value_function (line: String, regex: String ): String =
        line.replaceAll (regex, translator.translate (regex )  )
      Rec () .fold (translator.keys, initial_value, next_value_function ) }
}

case class Replacement_ (line: String ) extends Replacement

trait Replacer {
  import soda.lib.Rec

  def line: String

  def pattern: String

  def replacement: String

  lazy val replaced_text =
    postprocess (Rec () .fold (Rec () .range (line.length ), initial_value, next_value_function, should_continue ) )

  lazy val initial_value = ReplacerFoldTuple_ (Seq (), 0 )

  def next_value_function (tuple: ReplacerFoldTuple, x: Int ): ReplacerFoldTuple =
    _get_next_tuple (replaced_text_rev = tuple.replaced_text_rev, start_index = tuple.start_index, pos = line.indexOf (pattern, tuple.start_index )    )

  def _get_next_tuple (replaced_text_rev: Seq [String], start_index: Int, pos: Int ): ReplacerFoldTuple =
    if (pos == -1
    ) ReplacerFoldTuple_ (replaced_text_rev.+: (line.substring (start_index )  ), pos )
    else
      {
        lazy val new_replaced_text_rev = (replaced_text_rev.+: (line.substring (start_index, pos )  )  ) .+: (replacement )
        lazy val new_index = pos + pattern.length
        ReplacerFoldTuple_ (new_replaced_text_rev, new_index ) }

  def should_continue (tuple: ReplacerFoldTuple, x: Int ): Boolean =
    ! (tuple.start_index == -1 )

  def postprocess (tuple: ReplacerFoldTuple ): String =
    tuple.replaced_text_rev.reverse.mkString ("")
}

case class Replacer_ (line: String, pattern: String, replacement: String ) extends Replacer

trait ReplacerFoldTuple {

  def replaced_text_rev: Seq [String]

  def start_index: Int
}

case class ReplacerFoldTuple_ (replaced_text_rev: Seq [String], start_index: Int ) extends ReplacerFoldTuple
