package soda.example.forcoq.lib

case class NatSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("IntNat from non negative") (
    check (
      obtained = IntNat_ ().from_non_negative (8)
    ) (
      expected = S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (O_ ( ) ) ) ) ) ) ) ) )
    )
  )

  test ("IntNat to Int") (
    check (
      obtained = IntNat_ ().to_Int ( S_ (S_ (S_ (S_ (S_ (O_ ( ) ) ) ) ) ) )
    ) (
      expected = 5
    )
  )

  test ("Nat add") (
    check (
      obtained =  S_ (S_ (S_ (O_ ( ) ) ) ).add (S_ (S_ (S_ (S_ (S_ (O_ ( ) ) ) ) ) ) )
    ) (
      expected = S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (O_ ( ) ) ) ) ) ) ) ) )
    )
  )

  test ("Nat mul") (
    check (
      obtained = S_ (S_ (S_ (O_ ( ) ) ) ).mul (S_ (S_ (S_ (S_ (O_ ( ) ) ) ) ) )
    ) (
      expected = S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (S_ (O_ ( ) ) ) ) ) ) ) ) ) ) ) ) )
    )
  )

}
