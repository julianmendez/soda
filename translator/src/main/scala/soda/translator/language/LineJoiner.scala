package soda.translator.language


case class LineJoiner (lines: Seq [String]  ) {

  lazy val Comma = ","
  lazy val SodaOpeningParenthesis: String = "("
  lazy val SodaClosingParenthesis: String = ")"
  lazy val SodaOpeningBracket: String = "["
  lazy val SodaClosingBracket: String = "]"
  lazy val Space = " "

  lazy val get_joined_lines_with_forward_join: Seq [String] =
    Joiner (lines, condition_for_forward_join ) .join

  lazy val get_joined_lines_with_backward_join: Seq [String] =
    Joiner (lines, condition_for_backward_join ) .join

  def condition_for_forward_join (previous_line: String, current_line: String ): Boolean =
    condition_for_symbol_forward_join (previous_line ) ||
    condition_for_reserved_word_forward_join (previous_line )

  def condition_for_symbol_forward_join (previous_line: String ): Boolean =
    Translation () .SymbolForwardJoiner
      .exists (previous_line.endsWith )

  def condition_for_reserved_word_forward_join (previous_line: String ): Boolean =
    Translation () .ReservedWordJoiner
      .map (x => Space + x )
      .exists (previous_line.endsWith )

  def condition_for_backward_join (previous_line: String, current_line: String ): Boolean =
    condition_for_symbol_backward_join (current_line ) ||
    condition_for_reserved_word_backward_join (current_line )

  def condition_for_symbol_backward_join (current_line: String ): Boolean =
    Translation () .SymbolBackwardJoiner
      .exists (current_line.startsWith )

  def condition_for_reserved_word_backward_join (current_line: String ): Boolean =
    Translation () .ReservedWordJoiner
      .map (x => x + Space )
      .exists (current_line.startsWith )
}

case class Joiner (lines_to_join: Seq [String], condition_for_join: (String, String ) => Boolean ) {
  import soda.lib.Rec

  lazy val join: Seq [String] = reverse_join.reverse

  lazy val reverse_join: Seq [String] =
    if (lines_to_join.isEmpty
    ) lines_to_join
    else
      {
        lazy val tuple = Rec () .foldLeft (lines_to_join.tail, _initial_value (lines_to_join.head ), _next_value )
        lazy val result =
          if (tuple.in_process_rev.isEmpty
          ) tuple.processed_rev.+: (tuple.previous_line )
          else tuple.processed_rev.+: (_rev_list_as_element (tuple.in_process_rev, tuple.previous_line )  )
        result }

  def _initial_value (first_line: String ): JoinerFoldTuple =
    JoinerFoldTuple (Seq (), Seq (), first_line )

  def _next_value (tuple: JoinerFoldTuple, head: String ): JoinerFoldTuple =
    if (condition_for_join (tuple.previous_line.trim, head.trim )
    ) JoinerFoldTuple (tuple.in_process_rev.+: (tuple.previous_line ), tuple.processed_rev, head )
    else
      {
        lazy val processed_line = _rev_list_as_element (tuple.in_process_rev, tuple.previous_line )
        JoinerFoldTuple (Seq (), tuple.processed_rev.+: (processed_line ), head ) }

  def _rev_list_as_element (in_process_rev: Seq [String], line: String ): String =
    in_process_rev.reverse.mkString ("") + line
}

case class JoinerFoldTuple (in_process_rev: Seq [String], processed_rev: Seq [String], previous_line: String )
