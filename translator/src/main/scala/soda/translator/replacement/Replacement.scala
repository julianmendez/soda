package soda.translator.replacement

/**
 * This models a collection of replacement functions.
 * This is intended to be used as a pipeline.
 */
trait Replacement  extends soda.translator.block.SingleLineProcessor {

  import soda.translator.block.Translator

  lazy val aux = ReplacementAux_ ()

  lazy val soda_space = aux.soda_space

  def replace_with (function: String => String ): Replacement =
    Replacement_ (function (line )  )

  def replace_at_beginning (index: Int, translator: Translator ): Replacement =
    Replacement_ (aux.replace_at_beginning (line, index, translator )  )

  def replace_all (pattern: String, replacement: String ): Replacement =
    Replacement_ (aux.replace_all (line, pattern, replacement )  )

  def replace (translator: Translator ): Replacement =
    Replacement_ (aux.replace (line, translator )  )

  def add_space_to_soda_line (): Replacement =
    Replacement_ (soda_space + line + soda_space )

  def add_spaces_to_symbols (symbols: Set [Char]  ): Replacement =
    Replacement_ (aux.add_spaces_to_symbols (line, symbols )  )

  def remove_space_from_scala_line (): Replacement =
    Replacement_ (aux.remove_space_from_scala_line (line )  )

  def add_after_spaces_or_pattern (pattern: String, text_to_prepend: String ): Replacement =
    Replacement_ (aux.add_after_spaces_or_pattern (line, pattern, text_to_prepend )  )

  def replace_regex (translator: Translator ): Replacement =
    Replacement_ (aux.replace_regex (line, translator )  )

}

case class Replacement_ (line: String )  extends Replacement

trait ReplacementAux {

 import soda.translator.block.Translator
 import soda.lib.Recursion_

  lazy val soda_space = " "

  lazy val scala_space = " "

  def replace_at_beginning (line: String, index: Int, translator: Translator ): String =
    if (index == 0
    ) replace_only_beginning (line, translator )
    else line

  def replace_only_beginning (line: String, translator: Translator ): String =
    {
      lazy val initial_value: String = line
      def next_value_function (line: String, reserved_word: String ): String =
        replace_if_found_at_beginning (line, soda_space + reserved_word + soda_space, scala_space + translator.translate (reserved_word ) + scala_space )
      Recursion_ () .fold (translator.keys, initial_value, next_value_function ) }

  def replace_if_found_at_beginning (line: String, pattern: String, new_text: String ): String =
    if (line.trim.startsWith (pattern.trim )
    ) replace_all (line, pattern, new_text )
    else line

  def replace (line: String, translator: Translator ): String =
    {
      lazy val initial_value: String = line
      def next_value_function (line: String, reserved_word: String ): String =
        replace_if_found (line, soda_space + reserved_word + soda_space, scala_space + translator.translate (reserved_word ) + scala_space )
      Recursion_ () .fold (translator.keys, initial_value, next_value_function ) }

  def replace_if_found  (line: String, pattern: String, new_text: String ): String =
    if (line.contains  (pattern )
    ) replace_all  (line, pattern, new_text )
    else line

  def replace_all  (line: String, pattern: String, replacement: String ): String =
    Replacer_ (line, pattern, replacement ) .replaced_text

  def add_spaces_to_symbols  (line: String, symbols: Set [Char]  ): String =
    line.indices.map (index =>
      {
        lazy val ch = line  (index )
        _left_part_of_symbols  (line, symbols, index, ch ) + ch + _right_part_of_symbols  (line, symbols, index, ch ) }    ) .mkString ("")

  def _left_part_of_symbols  (line: String, symbols: Set [Char], index: Int, ch: Char ): String =
    if ((index > 0 ) && symbols.contains  (ch ) &&
      ! line  (index - 1 ) .isWhitespace
    ) scala_space
    else ""

  def _right_part_of_symbols  (line: String, symbols: Set [Char], index: Int, ch: Char ): String =
    if ((index < line.length - 1 ) && symbols.contains  (ch ) &&
      ! line  (index + 1 ) .isWhitespace
    ) scala_space
    else ""

  def remove_space_from_scala_line  (line: String ): String =
    _get_line_without_ending_space (_get_line_without_starting_space (line ) )

  def _get_line_without_starting_space (line: String ): String =
    if (line.startsWith (scala_space )
    ) line.substring (1 )
    else line

  def _get_line_without_ending_space (line: String ): String =
    if (line.endsWith (scala_space )
    ) line.substring (0, line.length - 1 )
    else line

  def add_after_spaces_or_pattern (line: String, pattern: String, text_to_prepend: String ): String =
    {
      lazy val prefix_length =
        if (line.trim.startsWith (pattern )
        ) line.indexOf (pattern ) + pattern.length
        else line.takeWhile (ch => ch.isSpaceChar ) .length
      line.substring (0, prefix_length ) + text_to_prepend + line.substring (prefix_length ) }

  def replace_regex (line: String, translator: Translator ): String =
    {
      lazy val initial_value: String = line
      def next_value_function (line: String, regex: String ): String =
        line.replaceAll (regex, translator.translate (regex )  )
      Recursion_ () .fold (translator.keys, initial_value, next_value_function ) }

}

case class ReplacementAux_ ()  extends ReplacementAux
