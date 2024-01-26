package soda.example.algorithms

/*
 * This package contains tests for examples of simple algorithms.
 */

trait Package

case class FizzBuzzSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val expected_result = Seq (
    "1" , "2" , "Fizz" , "4" , "Buzz" , "Fizz" , "7" , "8" , "Fizz" , "Buzz" ,
    "11" , "Fizz" , "13" , "14" , "FizzBuzz" , "16" , "17" , "Fizz" , "19" , "Buzz" ,
    "Fizz" , "22" , "23" , "Fizz" , "Buzz" , "26" , "Fizz" , "28" , "29" , "FizzBuzz" ,
    "31" , "32" , "Fizz" , "34" , "Buzz" , "Fizz" , "37" , "38" , "Fizz" , "Buzz" ,
    "41" , "Fizz" , "43" , "44" , "FizzBuzz" , "46" , "47" , "Fizz" , "49" , "Buzz" ,
    "Fizz" , "52" , "53" , "Fizz" , "Buzz" , "56" , "Fizz" , "58" , "59" , "FizzBuzz" ,
    "61" , "62" , "Fizz" , "64" , "Buzz" , "Fizz" , "67" , "68" , "Fizz" , "Buzz" ,
    "71" , "Fizz" , "73" , "74" , "FizzBuzz" , "76" , "77" , "Fizz" , "79" , "Buzz" ,
    "Fizz" , "82" , "83" , "Fizz" , "Buzz" , "86" , "Fizz" , "88" , "89" , "FizzBuzz" ,
    "91" , "92" , "Fizz" , "94" , "Buzz" , "Fizz" , "97" , "98" , "Fizz" , "Buzz"
  )

  private lazy val _fizz_buzz = FizzBuzz_ ()

  private lazy val _fizz_buzz_pattern_matching = FizzBuzzPatternMatching_ ()

  test ("first elements of FizzBuzz") (
    check (
      obtained = _fizz_buzz .apply
    ) (
      expected = expected_result
    )
  )

  test ("first elements of FizzBuzz with pattern matching") (
    check (
      obtained = _fizz_buzz_pattern_matching .apply
    ) (
      expected = expected_result
    )
  )

}


case class PatternMatchingSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance = PatternMatching_ ()

  test ("get value and name of singleton - 1") (
    check (
      obtained = instance .get_value (Singleton_ (5) )
    ) (
      expected = 5
    )
  )

  test ("get value and name of singleton - 2") (
    check (
      obtained = instance .get_type_name (Singleton_ (5) )
    ) (
      expected = "singleton (x)"
    )
  )

  test ("get value and name of pair - 1") (
    check (
      obtained = instance .get_value (Pair_ (10, 100) )
    ) (
      expected = 55
    )
  )

  test ("get value and name of pair - 2") (
    check (
      obtained = instance .get_type_name (Pair_ (10, 100) )
    ) (
      expected = "pair (x) (y)"
    )
  )

  test ("get value and name of triplet - 1") (
    check (
      obtained = instance .get_value (Triplet_ (9, 100, 890) )
    ) (
      expected = 333
    )
  )

  test ("get value and name of triplet - 2") (
    check (
      obtained = instance .get_type_name (Triplet_ (9, 100, 890) )
    ) (
      expected = "triplet (x) (y) (z)"
    )
  )

}


trait SaladIngredient
  extends
    soda.lib.EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class SaladIngredient_ (ordinal : Int, name : String) extends SaladIngredient

object SaladIngredient {
  def mk (ordinal : Int) (name : String) : SaladIngredient =
    SaladIngredient_ (ordinal, name)
}

trait SaladIngredientConstant
{

  private def _mk_SaladIngredient (ordinal : Int) (name : String) : SaladIngredient =
    SaladIngredient_ (ordinal, name)

  lazy val tomato = _mk_SaladIngredient (1) ("tomato")

  lazy val lettuce = _mk_SaladIngredient (2) ("lettuce")

  lazy val sunflower_seeds = _mk_SaladIngredient (3) ("sunflower seeds")

  lazy val olive_oil = _mk_SaladIngredient (4) ("olive_oil")

  lazy val SaladIngredient_values = Seq (tomato , lettuce , sunflower_seeds , olive_oil)

}

case class SaladIngredientConstant_ () extends SaladIngredientConstant

object SaladIngredientConstant {
  def mk : SaladIngredientConstant =
    SaladIngredientConstant_ ()
}

case class SaladMakerSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
    with SaladIngredientConstant
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  def add_next_ingredient (salad_so_far : Seq [SaladIngredient] ) (ingredient : SaladIngredient)
      : Seq [SaladIngredient] =
    salad_so_far .+: (ingredient)

  def has_salad_at_most_2_ingredients (salad_so_far : Seq [SaladIngredient] )
      (next_ingredient : SaladIngredient) : Boolean =
    salad_so_far .length < 3

  private lazy val _salad_maker = SaladMaker_ ()

  test ("salad maker") (
    check (
      obtained = _salad_maker .apply (
        list_of_ingredients = SaladIngredient_values) (
        initial_bowl = Seq [SaladIngredient] () ) (
        next_ingredient = add_next_ingredient) (
        condition = has_salad_at_most_2_ingredients
      )
    ) (
      expected = Seq (sunflower_seeds , lettuce , tomato)
    )
  )

}


case class SortExampleSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.lib.SomeSD_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val sorted_sequence = Seq (1 , 3 , 5 , 5 , 8 , 9)

  lazy val unsorted_sequence = Seq (1 , 3 , 5 , 4 , 8 , 9)

  test ("test sorted sequence with at") (
    check (
      obtained = SortExampleWithAt_ () .is_sorted (sorted_sequence)
    ) (
      expected = true
    )
  )

  test ("test unsorted sequence with at") (
    check (
      obtained = SortExampleWithAt_ () .is_sorted (unsorted_sequence)
    ) (
      expected = false
    )
  )

  test ("test sorted sequence with zip") (
    check (
      obtained = SortExampleWithZip_ () .is_sorted (sorted_sequence)
    ) (
      expected = true
    )
  )

  test ("test unsorted sequence with zip") (
    check (
      obtained = SortExampleWithZip_ () .is_sorted (unsorted_sequence)
    ) (
      expected = false
    )
  )

  test ("insert sorted simple") (
    check (
      obtained = SortAlgorithmExampleWithFold_ ()
        .insert_sorted (Seq (1 , 2 , 3 , 6 , 8 , 9) ) (5)
    ) (
      expected = Seq (1 , 2 , 3 , 5 , 6 , 8 , 9)
    )
  )

  test ("insert sorted with repetition") (
    check (
      obtained = SortAlgorithmExampleWithFold_ ()
        .insert_sorted (Seq (1 , 2 , 3 , 5 , 6 , 8 , 9) ) (5)
    ) (
      expected = Seq (1 , 2 , 3 , 5 , 5 , 6 , 8 , 9)
    )
  )

  test ("sort unsorted sequence") (
    check (
      obtained = SortAlgorithmExampleWithFold_ () .sort (Seq (3 , 5 , 1 , 9 , 8 , 4) )
    ) (
      expected = Seq (1 , 3 , 4 , 5 , 8 , 9)
    )
  )

  test ("sort unsorted sequence applying constraints to verify correctness") (
    check (
      obtained = ConstrainedSortAlgorithm_ () .sort (Seq (3 , 5 , 1 , 9 , 8 , 4) )
    ) (
      expected = SomeSD_ (Seq (1 , 3 , 4 , 5 , 8 , 9) )
    )
  )

  test ("sort unsorted sequence with SortedSequenceBuilder") (
    check (
      obtained =
        SortedSequenceBuilder_ [Integer] ()
          .build (Seq (3 , 5 , 1 , 9 , 8 , 4) .map ( x => Integer .valueOf (x) ) )
          .sequence
          .map ( x => x .intValue)
    ) (
      expected = Seq (1 , 3 , 4 , 5 , 8 , 9)
    )
  )

}

