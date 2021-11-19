package soda.translator.language



trait LineJoinerToScala  extends soda.translator.replacement.MultiLineProcessor {

  lazy val tc = TranslationConstantToScala_ ()

  lazy val comma = ","

  lazy val soda_opening_parenthesis: String = "("

  lazy val soda_closing_parenthesis: String = ")"

  lazy val soda_opening_bracket: String = "["

  lazy val soda_closing_bracket: String = "]"

  lazy val space = " "

  lazy val joined_lines_with_forward_join: Seq [String] =
    Joiner_ (lines, is_a_forward_join ) .join

  lazy val joined_lines_with_backward_join: Seq [String] =
    Joiner_ (lines, is_a_backward_join ) .join

  def is_a_forward_join (previous_line: String, current_line: String ): Boolean =
    is_a_symbol_forward_join (previous_line ) ||
    is_a_reserved_word_forward_join (previous_line )

  def is_a_symbol_forward_join (previous_line: String ): Boolean =
    tc.symbol_forward_joiner
      .exists (previous_line.endsWith )

  def is_a_reserved_word_forward_join (previous_line: String ): Boolean =
    tc.reserved_word_joiner
      .map (x => space + x )
      .exists (previous_line.endsWith )

  def is_a_backward_join (previous_line: String, current_line: String ): Boolean =
    is_a_symbol_backward_join (current_line ) ||
    is_a_reserved_word_backward_join (current_line )

  def is_a_symbol_backward_join (current_line: String ): Boolean =
    tc.symbol_backward_joiner
      .exists (current_line.startsWith )

  def is_a_reserved_word_backward_join (current_line: String ): Boolean =
    tc.reserved_word_joiner
      .map (x => x + space )
      .exists (current_line.startsWith )

}

case class LineJoinerToScala_ (lines: Seq [String]  )  extends LineJoinerToScala

trait Joiner {
  import soda.lib.Recursion_

  def lines_to_join: Seq [String]

  def is_a_join: (String, String ) => Boolean

  lazy val join: Seq [String] = reverse_join.reverse

  lazy val reverse_join: Seq [String] =
    if (lines_to_join.isEmpty
    ) lines_to_join
    else
      {
        lazy val tuple = Recursion_ () .fold (lines_to_join.tail, _initial_value_function (lines_to_join.head ), _next_value_function )
        lazy val result =
          if (tuple.in_process_rev.isEmpty
          ) tuple.processed_rev.+: (tuple.previous_line )
          else tuple.processed_rev.+: (_rev_list_as_element (tuple.in_process_rev, tuple.previous_line )  )
        result }

  def _initial_value_function (first_line: String ): JoinerFoldTuple =
    JoinerFoldTuple_ (Seq (), Seq (), first_line )

  def _next_value_function (tuple: JoinerFoldTuple, head: String ): JoinerFoldTuple =
    if (is_a_join (tuple.previous_line.trim, head.trim )
    ) JoinerFoldTuple_ (tuple.in_process_rev.+: (tuple.previous_line ), tuple.processed_rev, head )
    else
      {
        lazy val processed_line = _rev_list_as_element (tuple.in_process_rev, tuple.previous_line )
        JoinerFoldTuple_ (Seq (), tuple.processed_rev.+: (processed_line ), head ) }

  def _rev_list_as_element (in_process_rev: Seq [String], line: String ): String =
    in_process_rev.reverse.mkString ("") + line

}

case class Joiner_ (lines_to_join: Seq [String], is_a_join: (String, String ) => Boolean )  extends Joiner

trait JoinerFoldTuple {

  def in_process_rev: Seq [String]

  def processed_rev: Seq [String]

  def previous_line: String

}

case class JoinerFoldTuple_ (in_process_rev: Seq [String], processed_rev: Seq [String], previous_line: String )  extends JoinerFoldTuple
