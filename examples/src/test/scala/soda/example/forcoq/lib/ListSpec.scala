package soda.example.forcoq.lib

case class ListSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  test ("list from Seq")
    {
      lazy val instance = SeqList_ ()
      lazy val expected = (cons_ (0, cons_ (1, cons_ (1, cons_ (2, cons_ (3, cons_ (5, nil_ [Int] () ) ) ) ) ) ) )
      lazy val obtained =
        instance.from_Seq (Seq [Int] (0, 1, 1, 2, 3, 5 ) )
      assert (obtained == expected ) }

  test ("list to Seq")
    {
      lazy val instance = SeqList_ ()
      lazy val expected = Seq [Int] (1, 2, 4, 8, 16 )
      lazy val obtained =
        instance.to_Seq (
          (cons_ (1, cons_ (2, cons_ (4, cons_ (8, cons_ (16, nil_ [Int] () ) ) ) ) ) )
        )
      assert (obtained == expected ) }

}
