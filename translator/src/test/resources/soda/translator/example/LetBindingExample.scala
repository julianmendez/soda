package soda.translator.example


case class LetBindingExample (  ) {

  lazy val three_parts_like_where = {
    lazy val x = Seq ( first_part , second_part , third_part )
    /* where */
    lazy val first_part = "first" + part
    lazy val second_part = "second" + part
    lazy val third_part = "third" + part
    lazy val part = " part"
    x
  }

  lazy val three_parts_like_let_in = {
    /* let */
    lazy val first_part = "first" + part
    lazy val second_part = "second" + part
    lazy val third_part = "third" + part
    lazy val part = " part"
    /* in */
    Seq ( first_part , second_part , third_part )
  }

}
