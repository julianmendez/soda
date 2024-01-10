package soda.example.forlean.algorithms

/*
 * This package contains examples using recursion for Lean.
 */

import   soda.example.forlean.lib.IntNat_
import   soda.example.forlean.lib.Nat
import   soda.example.forlean.lib.Succ_
import   soda.example.forlean.lib.Zero_



trait Package

case class RecursionForLeanSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  def toNat (n : Int) : Nat =
    IntNat_ () .from_non_negative (n)

  lazy val example_seq : List [Int] = List (0 , 1 , 1 , 2 , 3 , 5 , 8)

  private lazy val _fold_left_while_initial_value = List [String] ()

  private lazy val _fold_left_while_next_value_function : List [String] => Int => List [String] =
     (s : List [String] ) =>  (e : Int) => s  .+: ("" +  (e + 100))

  private lazy val _fold_left_while_condition : List [String] => Int => Boolean =
     (s : List [String] ) =>  (e : Int) => e < 5

  test ("fold left while with Seq") (
    check (
      obtained =
        RecursionForLean_ ()
          .fold4 (example_seq) (_fold_left_while_initial_value) (
            _fold_left_while_next_value_function) (_fold_left_while_condition)
    ) (
      expected = List ("103" , "102" , "101" , "101" , "100")
    )
  )

  private lazy val _fold_left_initial_value = List [String] ()

  private lazy val _fold_left_next_value_function : List [String] => Int => List [String] =
     (s : List [String]) =>  (e : Int) => s .+: ("" + (e + 100))

  test ("fold left with Seq") (
    check (
      obtained =
        RecursionForLean_ ()
          .fold3 (example_seq) (_fold_left_initial_value) (
            _fold_left_next_value_function)
    ) (
      expected = List ("108" , "105" , "103" , "102" , "101" , "101" , "100")
    )
  )

  test ("range with positive number") (
    check (
      obtained = RecursionForLean_ () .range (toNat (8) )
    ) (
      expected = List [Nat] (toNat (0) , toNat (1) , toNat (2) , toNat (3) ,
       toNat (4) , toNat (5) , toNat (6) , toNat(7) )
    )
  )

  test ("range with zero size") (
    check (
      obtained = RecursionForLean_ () .range (toNat (0) )
    ) (
      expected = List [Nat] ()
    )
  )

}


case class SwapExampleSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val zero = Zero_ ()

  lazy val three = Succ_ (Succ_ (Succ_ (zero) ) )

  lazy val five = Succ_ (Succ_ (three) )

  lazy val pair_1 = PairExample_ (three, five)

  lazy val pair_2 = PairExample_ (five, three)

  lazy val instance = SwapExample_ ()

  test ("one swap") (
    check (
      obtained = instance .swap (pair_1)
    ) (
      expected = pair_2
    )
  )

  test ("two swaps") (
    check (
      obtained = instance .swap (instance .swap (pair_1) )
    ) (
      expected = pair_1
    )
  )

}

