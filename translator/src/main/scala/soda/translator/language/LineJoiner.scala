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
    {
      lazy val tuples = Rec () .foldLeft (lines_to_join, initial_value, next_value )
      lazy val result =
        if (tuples.in_process_rev.isEmpty
        ) tuples.processed_rev
        else tuples.processed_rev.+: (_rev_list_as_element (tuples.in_process_rev, "")  )
      result }

  lazy val initial_value = FoldTuple (Seq (), Seq (), "")

  def next_value (pair: FoldTuple, head: String ): FoldTuple =
    if (condition_for_forward_join (head.trim )
    ) FoldTuple (pair.in_process_rev.+: (head ), pair.processed_rev, head )
    else
      {
        lazy val new_head = _rev_list_as_element (pair.in_process_rev, head )
        FoldTuple (Seq (), pair.processed_rev.+: (new_head ), head ) }

  def _rev_list_as_element (in_process_rev: Seq [String], line: String ): String =
    in_process_rev.reverse.mkString ("") + line

  case class FoldTuple (in_process_rev: Seq [String], processed_rev: Seq [String], previous_line: String )
}
