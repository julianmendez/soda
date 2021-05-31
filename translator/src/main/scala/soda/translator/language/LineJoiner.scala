package soda.translator.language


case class LineJoiner (lines: Seq [String]  ) {
  import soda.lib.Rec
  import soda.lib.SomeSD

  lazy val Comma = ","
  lazy val SodaOpeningParenthesis: String = "("
  lazy val SodaClosingParenthesis: String = ")"
  lazy val SodaOpeningBracket: String = "["
  lazy val SodaClosingBracket: String = "]"

  lazy val get_joined_lines_with_opening_brackets: Seq [String] =
    Joiner (lines, condition_for_forward_join ) .join

  lazy val get_joined_lines_with_closing_brackets: Seq [String] =
    Joiner (lines, condition_for_backward_join ) .join

  def condition_for_forward_join (previous_line: String, current_line: String ): Boolean =
    previous_line.endsWith (Comma ) ||
      previous_line.endsWith (SodaOpeningParenthesis ) ||
      previous_line.endsWith (SodaOpeningBracket )

  def condition_for_backward_join (previous_line: String, current_line: String ): Boolean =
    current_line.startsWith (SodaClosingParenthesis ) ||
      current_line.startsWith (SodaClosingBracket )

  case class Joiner (lines_to_join: Seq [String], condition_for_join: (String, String ) => Boolean ) {

    lazy val join: Seq [String] = reverse_join.reverse

    lazy val reverse_join: Seq [String] =
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
      if (condition_for_join (tuple.previous_line.trim, head.trim )
      ) FoldTuple (tuple.in_process_rev.+: (tuple.previous_line ), tuple.processed_rev, head )
      else
        {
          lazy val processed_line = _rev_list_as_element (tuple.in_process_rev, tuple.previous_line )
          FoldTuple (Seq (), tuple.processed_rev.+: (processed_line ), head ) }

    def _rev_list_as_element (in_process_rev: Seq [String], line: String ): String =
      in_process_rev.reverse.mkString ("") + line

    case class FoldTuple (in_process_rev: Seq [String], processed_rev: Seq [String], previous_line: String )
  }
}
