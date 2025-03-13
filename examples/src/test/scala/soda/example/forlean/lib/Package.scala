package soda.example.forlean.lib

/*
 * This package contains tests for helper classes for a translation to Lean.
 */

case class MyListSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  def toNat (n : Int) : Nat =
    IntNat .mk .from_non_negative (n)

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
        MyList .mk
          .foldl [Int, List [String] ] (example_list) (_fold_left_initial_value) (
            _fold_left_next_value_function)
    ) (
      expected = List ("108" , "105" , "103" , "102" , "101" , "101" , "100")
    )
  )

  test ("length of a list") (
    check (
      obtained = MyList .mk .length (example_list)
    ) (
      expected = toNat (7)
    )
  )

  test ("length of an empty list") (
    check (
      obtained = MyList .mk .length (empty_int_list)
    ) (
      expected = toNat (0)
    )
  )

  test ("reverse of a list") (
    check (
      obtained =
        MyList .mk
          .reverse [String] (initial_string_list)
    ) (
      expected = List [String] ("13" , "8" , "5" , "3" , "2" , "1" , "1", "0")
    )
  )

  test ("reverse of an empty list") (
    check (
      obtained =
        MyList .mk
          .reverse [String] (empty_string_list)
    ) (
      expected = List [String] ()
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
      obtained = IntNat .mk .from_non_negative (8)
    ) (
      expected = Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Zero .mk ) ) ) ) ) ) ) )
    )
  )

  test ("IntNat to Int") (
    check (
      obtained = IntNat .mk .to_Int (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Zero .mk ) ) ) ) ) )
    ) (
      expected = 5
    )
  )

  test ("Nat add") (
    check (
      obtained =  Succ_ (Succ_ (Succ_ (Zero .mk ) ) ) .add (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Zero .mk ) ) ) ) ) )
    ) (
      expected = Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Zero .mk ) ) ) ) ) ) ) )
    )
  )

  test ("Nat mul") (
    check (
      obtained = Succ_ (Succ_ (Succ_ (Zero .mk ) ) ) .mul (Succ_ (Succ_ (Succ_ (Succ_ (Zero .mk ) ) ) ) )
    ) (
      expected = Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Succ_ (Zero .mk ) ) ) ) ) ) ) ) ) ) ) )
    )
  )

}


case class SimpleListSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("list from Seq") (
    check (
      obtained = SeqList .mk .from_Seq (Seq [Int] (0 , 1 , 1 , 2 , 3 , 5) )
    ) (
      expected = (cons_ (0 , cons_ (1 , cons_ (1 , cons_ (2 , cons_ (3 ,
        cons_ (5 , nil_ [Int] () ) ) ) ) ) ) )
    )
  )

  test ("list to Seq") (
    check (
      obtained = SeqList .mk .to_Seq ( (cons_ (1 , cons_ (2 , cons_ (4 , cons_ (8 ,
        cons_ (16 , nil_ [Int] () ) ) ) ) ) ) )
    ) (
      expected = Seq [Int] (1 , 2 , 4 , 8 , 16)
    )
  )

}

