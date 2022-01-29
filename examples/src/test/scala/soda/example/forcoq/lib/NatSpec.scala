package soda.example.forcoq.lib

case class NatSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  test ("IntNat from non negative")
    {
      lazy val instance = IntNat_ ()
      lazy val expected = S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (O_ () ) ) ) ) ) ) ) )
      lazy val obtained =
        instance.from_non_negative (8 )
      assert (obtained == expected ) }

  test ("IntNat to Int")
    {
      lazy val instance = IntNat_ ()
      lazy val expected = 5
      lazy val obtained =
        instance.to_Int (
          S_ (S_ (S_ (S_ (S_ (O_ () ) ) ) ) )
        )
      assert (obtained == expected ) }

  test ("Nat add")
    {
      lazy val a = S_ (S_ (S_ (O_ () ) ) )
      lazy val b = S_ (S_ (S_ (S_ (S_ (O_ () ) ) ) ) )
      lazy val expected = S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (O_ () ) ) ) ) ) ) ) )
      lazy val obtained = a.add (b )
      assert (obtained == expected ) }

  test ("Nat mul")
    {
      lazy val a = S_ (S_ (S_ (O_ () ) ) )
      lazy val b = S_ (S_ (S_ (S_ (O_ () ) ) ) )
      lazy val expected = S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (O_ () ) ) ) ) ) ) ) ) ) ) ) )
      lazy val obtained = a.mul (b )
      assert (obtained == expected ) }

}
