package soda.translator.replacement

/*
 * This package contains auxiliary classes for string manipulation,
 * especially related to replacement.
 */





/**
 * This models a collection of replacement functions.
 * This is intended to be used as a pipeline.
 */

trait Replacement
  extends
    soda.translator.block.SingleLineProcessor
{

  def   line : String

  import   soda.translator.block.Translator

  lazy val aux = ReplacementAux_ ()

  lazy val soda_space = aux.soda_space

  def replace_at_beginning (index : Int) (translator : Translator) : Replacement =
    Replacement_ (ReplacementWithTranslator_ (translator).replace_at_beginning (line) (index) )

  def replace_all (pattern : String) (replacement : String) : Replacement =
    Replacement_ (aux.replace_all (line) (pattern) (replacement) )

  def add_space_to_soda_line () : Replacement =
    Replacement_ (soda_space + line + soda_space)

  def remove_space_from_scala_line () : Replacement =
    Replacement_ (aux.remove_space_from_scala_line (line) )

  def add_after_spaces_or_pattern (pattern : String) (text_to_prepend : String) : Replacement =
    Replacement_ (aux.add_after_spaces_or_pattern (line) (pattern) (text_to_prepend) )

}

case class Replacement_ (line : String) extends Replacement

trait ReplacementAux
{

  lazy val soda_space = " "

  lazy val scala_space = " "

  def replace_if_found_at_beginning (line : String) (pattern : String) (new_text : String) : String =
    if ( line.trim.startsWith (pattern.trim)
    ) replace_first (line) (pattern) (new_text)
    else line

  def replace_first (line : String) (pattern : String) (replacement : String) : String =
    replace_at (line.indexOf (pattern) ) (line) (pattern) (replacement)

  def replace_at (index : Int) (line : String) (pattern : String) (replacement : String) : String =
    if ( (0 <= index) && (index + pattern.length <= line.length)
    ) line.substring (0, index) + replacement + line.substring (index + pattern.length, line.length)
    else line

  def replace_if_found (line : String) (pattern : String) (new_text : String) : String =
    if ( line.contains (pattern)
    ) replace_all (line) (pattern) (new_text)
    else line

  def replace_all (line : String) (pattern : String) (replacement : String) : String =
    Replacer_ (line, pattern, replacement).replaced_text

  def add_spaces_to_symbols (line : String) (symbols : Set [Char] ) : String =
    line
      .indices
      .map (  index => _add_spaces_to_symbols_with (line (index) ) (index ) (line) (symbols) )
      .mkString ("")

  private def _add_spaces_to_symbols_with (ch : Char) (index : Int) (line : String) (symbols : Set [Char] ) : String =
    (_left_part_of_symbols (line) (symbols) (index) (ch) ) + ch + (_right_part_of_symbols (line) (symbols) (index) (ch) )

  private def _left_part_of_symbols (line : String) (symbols : Set [Char] ) (index : Int) (ch : Char) : String =
    if ( (index > 0) && symbols.contains (ch) &&
      ! line (index - 1).isWhitespace
    ) scala_space
    else ""

  private def _right_part_of_symbols (line : String) (symbols : Set [Char] ) (index : Int) (ch : Char) : String =
    if ( (index < line.length - 1) && symbols.contains (ch) &&
      ! line (index + 1).isWhitespace
    ) scala_space
    else ""

  def remove_space_from_scala_line (line : String) : String =
    _get_line_without_ending_space ( _get_line_without_starting_space (line) )

  private def _get_line_without_starting_space (line : String) : String =
    if ( line.startsWith (scala_space)
    ) line.substring (1)
    else line

  private def _get_line_without_ending_space (line : String) : String =
    if ( line.endsWith (scala_space)
    ) line.substring (0, line.length - 1)
    else line

  def add_after_spaces_or_pattern (line : String) (pattern : String) (text_to_prepend : String) : String =
    _add_after_spaces_or_pattern_with (_get_prefix_length (line) (pattern) ) (line) (pattern) (text_to_prepend)

  private def _add_after_spaces_or_pattern_with (prefix_length : Int) (line : String) (pattern : String) (text_to_prepend : String) : String =
    line.substring (0, prefix_length) + text_to_prepend + line.substring (prefix_length)

  private def _get_prefix_length (line : String) (pattern : String) : Int =
    if ( line.trim.startsWith (pattern)
    ) line.indexOf (pattern) + pattern.length
    else line.takeWhile (  ch => ch.isSpaceChar).length

}

case class ReplacementAux_ () extends ReplacementAux

trait ReplacementWithTranslator
{

  def   translator : soda.translator.block.Translator

  import   soda.lib.Fold_

  lazy val aux = ReplacementAux_ ()

  lazy val soda_space = aux.soda_space

  lazy val scala_space = aux.scala_space

  private lazy val _fold = Fold_ ()

  def replace_at_beginning (line : String) (index : Int) : String =
    if ( index == 0
    ) _replace_only_beginning (line)
    else line

  private def _replace_only_beginning (line : String) : String =
    _fold.apply (translator.keys) (initial_value = line) (next_value_function = _next_replace_only_beginning)

  private def _next_replace_only_beginning (line : String) (reserved_word : String) : String =
    aux.replace_if_found_at_beginning (line) (
      soda_space + reserved_word + soda_space) (scala_space + translator.translate (reserved_word) + scala_space)

  def replace (line : String) : String =
    _fold.apply (translator.keys) (initial_value = line) (next_value_function = _next_replace)

  private def _next_replace (line : String) (reserved_word : String) : String =
    aux.replace_if_found (line) (
      soda_space + reserved_word + soda_space) (scala_space + translator.translate (reserved_word) + scala_space)

  def replace_regex (line : String) : String =
    _fold.apply (translator.keys) (initial_value = line) (next_value_function = _next_replace_regex)

  private def _next_replace_regex (line : String) (regex : String) : String =
    line.replaceAll (regex, translator.translate (regex) )

}

case class ReplacementWithTranslator_ (translator : soda.translator.block.Translator) extends ReplacementWithTranslator
