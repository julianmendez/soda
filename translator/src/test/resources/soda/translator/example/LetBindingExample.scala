package soda.translator.example


case class LetBindingExample () {

  lazy val three_parts_like_where =
    {
      lazy val result = Seq (first_part, second_part, third_part )
      /* where */
      lazy val first_part = "first" + part
      lazy val second_part = "second" + part
      lazy val third_part = "third" + part
      lazy val part = " part"
      result  }

  lazy val three_parts_like_let_in =
    {
      lazy val first_part = "first" + part
      lazy val second_part = "second" + part
      lazy val third_part = "third" + part
      lazy val part = " part"
      Seq (first_part, second_part, third_part )  }
}
