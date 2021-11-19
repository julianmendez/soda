package soda.example.forcoq.lib


case class ListSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("list from Seq")
    {
      lazy val instance = SeqList_ ()
      lazy val expected = (cons (0, cons (1, cons (1, cons (2, cons (3, cons (5, nil [Int]  () ) ) ) ) ) ) )
      lazy val obtained =
        instance.from_Seq (Seq [Int]  (0, 1, 1, 2, 3, 5 ) )
      assert (obtained == expected ) }

  test ("list to Seq")
    {
      lazy val instance = SeqList_ ()
      lazy val expected = Seq [Int]  (1, 2, 4, 8, 16 )
      lazy val obtained =
        instance.to_Seq ((cons (1, cons (2, cons (4, cons (8, cons (16, nil [Int]  () ) ) ) ) ) )        )
      assert (obtained == expected ) }

}
