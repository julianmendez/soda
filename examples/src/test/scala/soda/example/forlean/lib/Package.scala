package soda.example.forlean.lib

/*
 * This package contains tests for helper classes for a translation to Lean.
 */

trait Package

case class ListSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("list from Seq") (
    check (
      obtained = SeqList_ () .from_Seq (Seq [Int] (0 , 1 , 1 , 2 , 3 , 5) )
    ) (
      expected = (cons_ (0 , cons_ (1 , cons_ (1 , cons_ (2 , cons_ (3 ,
        cons_ (5 , nil_ [Int] () ) ) ) ) ) ) )
    )
  )

  test ("list to Seq") (
    check (
      obtained = SeqList_ () .to_Seq ( (cons_ (1 , cons_ (2 , cons_ (4 , cons_ (8 ,
        cons_ (16 , nil_ [Int] () ) ) ) ) ) ) )
    ) (
      expected = Seq [Int] (1 , 2 , 4 , 8 , 16)
    )
  )

}


case class NatSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("IntNat from non negative") (
    check (
      obtained = IntNat_ () .from_non_negative (8)
    ) (
      expected = Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Zero_ () ) ) ) ) ) ) ) )
    )
  )

  test ("IntNat to Int") (
    check (
      obtained = IntNat_ () .to_Int (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Zero_ () ) ) ) ) ) )
    ) (
      expected = 5
    )
  )

  test ("Nat add") (
    check (
      obtained =  Succ_ (Succ_ (Succ_ (Zero_ () ) ) ) .add (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Zero_ () ) ) ) ) ) )
    ) (
      expected = Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Zero_ () ) ) ) ) ) ) ) )
    )
  )

  test ("Nat mul") (
    check (
      obtained = Succ_ (Succ_ (Succ_ (Zero_ () ) ) ) .mul (Succ_ (Succ_ (Succ_ (Succ_ (Zero_ () ) ) ) ) )
    ) (
      expected = Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Zero_ () ) ) ) ) ) ) ) ) ) ) ) )
    )
  )

}
