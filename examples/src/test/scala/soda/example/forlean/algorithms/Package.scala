package soda.example.forlean.algorithms

/*
 * This package contains examples using recursion for Lean.
 */

import   soda.example.forlean.lib.IntNat_
import   soda.example.forlean.lib.Nat
import   soda.example.forlean.lib.Succ_
import   soda.example.forlean.lib.Zero_



trait Package

case class MyListSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  def toNat (n : Int) : Nat =
    IntNat_ () .from_non_negative (n)

  lazy val example_list : List [Int] = List (0 , 1 , 1 , 2 , 3 , 5 , 8)

  lazy val empty_int_list : List [Int] = List ()

  lazy val initial_string_list : List [String] = List ("0", "1", "1", "2", "3", "5", "8", "13")

  lazy val empty_string_list : List [String] = List ()

  private lazy val _fold_left_initial_value = List [String] ()

  private lazy val _fold_left_next_value_function : List [String] => Int => List [String] =
     (s : List [String] ) =>  (e : Int) => s .+: ("" + (e + 100))

  test ("fold left with a list") (
    check (
      obtained =
        MyList_ ()
          .foldl [Int, List [String] ] (example_list) (_fold_left_initial_value) (
            _fold_left_next_value_function)
    ) (
      expected = List ("108" , "105" , "103" , "102" , "101" , "101" , "100")
    )
  )

  test ("length of a list") (
    check (
      obtained = MyList_ () .length (example_list)
    ) (
      expected = toNat (7)
    )
  )

  test ("length of an empty list") (
    check (
      obtained = MyList_ () .length (empty_int_list)
    ) (
      expected = toNat (0)
    )
  )

  test ("reverse of a list") (
    check (
      obtained =
        MyList_ ()
          .reverse [String] (initial_string_list)
    ) (
      expected = List [String] ("13" , "8" , "5" , "3" , "2" , "1" , "1", "0")
    )
  )

  test ("reverse of an empty list") (
    check (
      obtained =
        MyList_ ()
          .reverse [String] (empty_string_list)
    ) (
      expected = List [String] ()
    )
  )

}


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

  lazy val zero = 0

  lazy val three = 3

  lazy val five = 5

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

