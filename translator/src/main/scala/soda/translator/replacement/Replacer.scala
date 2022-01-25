package soda.translator.replacement

trait LinePatternProcessor {

  def   line: String
  def   pattern: String
  def   replacement: String

}

trait Replacer
  extends LinePatternProcessor {

  import   soda.lib.Recursion_

  lazy val replaced_text =
    postprocess (Recursion_ () .fold (Recursion_ () .range (line.length ), initial_value, next_value_function, should_continue ) )

  lazy val initial_value = ReplacerFoldTuple_ (Seq (), 0 )

  def next_value_function (tuple: ReplacerFoldTuple, x: Int ): ReplacerFoldTuple =
    _get_next_tuple (
      replaced_text_rev = tuple.replaced_text_rev,
      start_index = tuple.start_index,
      pos = line.indexOf (pattern, tuple.start_index )
    )

  def _get_next_tuple (replaced_text_rev: Seq [String], start_index: Int, pos: Int ): ReplacerFoldTuple =
    if (pos == -1
    ) ReplacerFoldTuple_ (replaced_text_rev.+: (line.substring (start_index )  ), pos )
    else
      ReplacerFoldTuple_ (
        (replaced_text_rev.+: (line.substring (start_index, pos )  )  ) .+: (replacement ),
        pos + pattern.length
      )

  def should_continue (tuple: ReplacerFoldTuple, x: Int ): Boolean =
    !  (tuple.start_index == -1 )

  def postprocess (tuple: ReplacerFoldTuple ): String =
    tuple.replaced_text_rev.reverse.mkString ("")

}

case class Replacer_ (line: String, pattern: String, replacement: String ) extends Replacer

trait ReplacerFoldTuple {

  def   replaced_text_rev: Seq [String]
  def   start_index: Int

}

case class ReplacerFoldTuple_ (replaced_text_rev: Seq [String], start_index: Int ) extends ReplacerFoldTuple
