package soda.translator.blocktr

trait Joiner {

  import soda.lib.Recursion_

  def lines_to_join: Seq [String]

  def is_a_join: (String, String ) => Boolean

  lazy val join: Seq [String] = reverse_join.reverse

  lazy val reverse_join: Seq [String] =
    if (lines_to_join.isEmpty
    ) lines_to_join
    else _process_reverse_join (Recursion_ () .fold (lines_to_join.tail, _initial_value_function (lines_to_join.head ), _next_value_function )  )

  def _process_reverse_join (tuple: JoinerFoldTuple ): Seq [String] =
    if (tuple.in_process_rev.isEmpty
    ) tuple.processed_rev.+: (tuple.previous_line )
    else _process_recursively (tuple )

  def _process_recursively (tuple: JoinerFoldTuple ): Seq [String] =
    tuple.processed_rev.+: (_rev_list_as_element (tuple.in_process_rev, tuple.previous_line )  )

  def _initial_value_function (first_line: String ): JoinerFoldTuple =
    JoinerFoldTuple_ (Seq (), Seq (), first_line )

  def _next_value_function (tuple: JoinerFoldTuple, head: String ): JoinerFoldTuple =
    if (is_a_join (tuple.previous_line.trim, head.trim )
    ) JoinerFoldTuple_ (tuple.in_process_rev.+: (tuple.previous_line ), tuple.processed_rev, head )
    else JoinerFoldTuple_ (Seq (), _process_recursively (tuple ), head )

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
