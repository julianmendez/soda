package soda.example

trait LetBindingExample {

  lazy val three_parts_like_where =
    {
      lazy val result = Seq (first_part, second_part, third_part )
      /* where */
      lazy val first_part = "first" + part
      lazy val second_part = "second" + part
      lazy val third_part = "third" + part
      lazy val part = " part"
      result }

  lazy val three_parts_like_let_in =
    {
      lazy val first_part = "first" + part
      lazy val second_part = "second" + part
      lazy val third_part = "third" + part
      lazy val part = " part"
      Seq (first_part, second_part, third_part ) }

  lazy val three_parts_like_coq_let_in =
    { lazy val part = " part"
     lazy val first_part = "first" + part
     
      lazy val second_part = "second" + part
     lazy val third_part = "third" + part
      Seq [String]  () .+: (third_part ) .+: (second_part ) .+: (first_part ) }

}

case class LetBindingExample_ ()  extends LetBindingExample
