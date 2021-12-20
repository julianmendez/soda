package soda.translator.blocktr

trait LineJoinerToScala  extends soda.translator.block.MultiLineProcessor {

  lazy val reserved_word_joiner: Seq [String] = Seq ("extends", "with"  )

  lazy val symbol_forward_joiner: Seq [String] = Seq (",", "(", "["  )

  lazy val symbol_backward_joiner: Seq [String] = Seq (")", "]"  )

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
    symbol_forward_joiner
      .exists (previous_line.endsWith )

  def is_a_reserved_word_forward_join (previous_line: String ): Boolean =
    reserved_word_joiner
      .map (x => space + x )
      .exists (previous_line.endsWith )

  def is_a_backward_join (previous_line: String, current_line: String ): Boolean =
    is_a_symbol_backward_join (current_line ) ||
    is_a_reserved_word_backward_join (current_line )

  def is_a_symbol_backward_join (current_line: String ): Boolean =
    symbol_backward_joiner
      .exists (current_line.startsWith )

  def is_a_reserved_word_backward_join (current_line: String ): Boolean =
    reserved_word_joiner
      .map (x => x + space )
      .exists (current_line.startsWith )

}

case class LineJoinerToScala_ (lines: Seq [String]  )  extends LineJoinerToScala
