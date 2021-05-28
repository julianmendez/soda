package soda.translator.language


case class LineJoiner (lines: Seq [String]  ) {
  import soda.lib.Rec

  lazy val Comma = ","
  lazy val SodaOpeningParenthesis: String = "("

  lazy val get_joined_lines =
    processed_lines.reverse

  lazy val processed_lines =
    {
      lazy val pairs = Rec () .foldLeft (lines, initial_value, next_value )
      lazy val result =
        if (pairs.in_process_rev.isEmpty
        ) pairs.processed_rev
        else pairs.processed_rev.+: (rev_list_as_element (pairs.in_process_rev, "")  )
      result  }

  lazy val initial_value = FoldTuple (Seq (), Seq ()  )

  def next_value (pair: FoldTuple, head: String ): FoldTuple =
    if (head.trim () .endsWith (Comma ) || head.trim () .endsWith (SodaOpeningParenthesis )
    ) FoldTuple (pair.in_process_rev.+: (head ), pair.processed_rev )
    else
      {
        lazy val new_head = rev_list_as_element (pair.in_process_rev, head )
        FoldTuple (Seq (), pair.processed_rev.+: (new_head )  )  }

  def rev_list_as_element (in_process_rev: Seq [String], line: String ): String =
    in_process_rev.reverse.mkString ("") + line

  case class FoldTuple (in_process_rev: Seq [String], processed_rev: Seq [String]  )
}
