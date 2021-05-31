package soda.translator.language


case class LineJoiner (lines: Seq [String]  ) {
  import soda.lib.Rec
  import soda.lib.SomeSD

  lazy val Comma = ","
  lazy val SodaOpeningParenthesis: String = "("
  lazy val SodaClosingParenthesis: String = ")"
  lazy val SodaOpeningBracket: String = "["
  lazy val SodaClosingBracket: String = "]"

  lazy val get_joined_lines: Seq [String] =
    reverse_join (lines ) .reverse

  def condition_to_join (previous_line: String, current_line: String ): Boolean =
    condition_for_forward_join (previous_line ) || condition_for_backward_join (current_line )

  def condition_for_forward_join (line: String ): Boolean =
    line.endsWith (Comma ) ||
      line.endsWith (SodaOpeningParenthesis ) ||
      line.endsWith (SodaOpeningBracket )

  def condition_for_backward_join (line: String ): Boolean =
    line.startsWith (SodaClosingParenthesis ) ||
      line.startsWith (SodaClosingBracket )

  def reverse_join (lines_to_join: Seq [String]  ): Seq [String] =
    if (lines_to_join.isEmpty
    ) lines_to_join
    else
      {
        lazy val tuple = Rec () .foldLeft (lines_to_join.tail, initial_value (lines_to_join.head ), next_value )
        lazy val result =
          if (tuple.in_process_rev.isEmpty
          ) tuple.processed_rev.+: (tuple.previous_line )
          else tuple.processed_rev.+: (_rev_list_as_element (tuple.in_process_rev, tuple.previous_line )  )
        result }

  def initial_value (first_line: String ): FoldTuple = FoldTuple (Seq (), Seq (), first_line )

  def next_value (tuple: FoldTuple, head: String ): FoldTuple =
    if (condition_for_forward_join (tuple.previous_line.trim )
    ) FoldTuple (tuple.in_process_rev.+: (tuple.previous_line ), tuple.processed_rev, head )
    else
      {
        lazy val processed_line = _rev_list_as_element (tuple.in_process_rev, tuple.previous_line )
        FoldTuple (Seq (), tuple.processed_rev.+: (processed_line ), head ) }

  def _rev_list_as_element (in_process_rev: Seq [String], line: String ): String =
    in_process_rev.reverse.mkString ("") + line

  case class FoldTuple (in_process_rev: Seq [String], processed_rev: Seq [String], previous_line: String )
}
