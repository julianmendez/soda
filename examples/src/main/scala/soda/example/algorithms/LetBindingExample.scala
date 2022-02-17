package soda.example.algorithms

trait LetBindingExample
{

  lazy val three_parts_like_where =
    {
      lazy val result = Seq (first_part, second_part, third_part)
      /* where */
      lazy val part = " part"
      lazy val first_part = "first" + part
      lazy val second_part = "second" + part
      lazy val third_part = "third" + part
     result }

  lazy val three_parts_like_let_in =
    {
      lazy val part = " part"
      lazy val first_part = "first" + part
      lazy val second_part = "second" + part
      lazy val third_part = "third" + part
     Seq (first_part, second_part, third_part) }

  lazy val three_parts_like_coq_let_in =
    { lazy val part = " part"
     lazy val first_part = "first" + part
     
      lazy val second_part = "second" + part
     lazy val third_part = "third" + part
     Seq [String] ().+: (third_part).+: (second_part).+: (first_part) }

  lazy val three_parts_without_let_in =
    Seq [String] ().+: (_third_part).+: (_second_part).+: (_first_part)

  lazy val _part = " part"

  lazy val _first_part = "first" + _part

  lazy val _second_part = "second" + _part

  lazy val _third_part = "third" + _part

}

case class LetBindingExample_ () extends LetBindingExample
