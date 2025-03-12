package soda.tiles.fairness.tile

/*
 * This package contains tests for the classes to model a resource allocation scenario.
 */

import   org.scalatest.funsuite.AnyFunSuite
import   soda.tiles.fairness.tool.Actor
import   soda.tiles.fairness.tool.Assignment
import   soda.tiles.fairness.tool.Context
import   soda.tiles.fairness.tool.Measure
import   soda.tiles.fairness.tool.Outcome
import   soda.tiles.fairness.tool.Outcome_
import   soda.tiles.fairness.tool.Resource
import   soda.tiles.fairness.tool.TileMessage
import   soda.tiles.fairness.tool.TileMessageBuilder
import   soda.tiles.fairness.tool.Random

case class EqualityTileSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example = ResourceAllocationScenarioExample .mk

  lazy val equality_tile = EqualityTile .mk (example .measure_sum) (example .resource_height)

  test ("equality on outcome 0") (
    check (
      obtained = equality_tile .apply (example .initial0) .contents
    ) (
      expected = false
    )
  )

  test ("equality on outcome 1") (
    check (
      obtained = equality_tile .apply (example .initial1) .contents
    ) (
      expected = true
    )
  )

  test ("equality on outcome 2") (
    check (
      obtained = equality_tile .apply (example .initial2) .contents
    ) (
      expected = true
    )
  )

  test ("equality on outcome 3") (
    check (
      obtained = equality_tile .apply (example .initial3) .contents
    ) (
      expected = true
    )
  )

}


case class EquityTileSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example = ResourceAllocationScenarioExample .mk

  lazy val equity_tile =
    EquityTile .mk (example .measure_sum) (example .actor_need) (example .resource_height)

  test ("equity on outcome 0") (
    check (
      obtained = equity_tile .apply (example .initial0) .contents
    ) (
      expected = true
    )
  )

  test ("equity on outcome 1") (
    check (
      obtained = equity_tile .apply (example .initial1) .contents
    ) (
      expected = false
    )
  )

  test ("equity on outcome 2") (
    check (
      obtained = equity_tile .apply (example .initial2) .contents
    ) (
      expected = false
    )
  )

  test ("equity on outcome 3") (
    check (
      obtained = equity_tile .apply (example .initial3) .contents
    ) (
      expected = true
    )
  )

}


trait ResourceAllocationScenarioExample
{



  def measure_sum (a : Measure) (b : Measure) : Measure =
    Measure .mk (a .value + b .value)

  lazy val resource0 = Resource .mk ("small box - 0.1 m")

  lazy val resource1 = Resource .mk ("medium box - 0.2 m")

  lazy val resource2 = Resource .mk ("large box - 0.3 m")

  lazy val actor0 = Actor .mk ("Anna A")

  lazy val actor1 = Actor .mk ("Bob B")

  lazy val actor2 = Actor .mk ("Charlie C")

  lazy val actor_need_map : Map [Actor, Measure] = Seq (
    Tuple2 [Actor, Measure] (actor0 , Measure .mk (30) ) ,
    Tuple2 [Actor, Measure] (actor1 , Measure .mk (10) ) ,
    Tuple2 [Actor, Measure] (actor2 , Measure .mk (0) )
  ) .toMap

  lazy val resource_height_map : Map [Resource, Measure] = Seq (
    Tuple2 [Resource, Measure] (resource0 , Measure .mk (10) ) ,
    Tuple2 [Resource, Measure] (resource1 , Measure .mk (20) ) ,
    Tuple2 [Resource, Measure] (resource2 , Measure .mk (30) )
  ) .toMap

  def actor_need (actor : Actor) : Measure =
    actor_need_map .getOrElse (actor , Measure .mk (-1) )

  def resource_height (resource : Resource) : Measure =
    resource_height_map .getOrElse (resource , Measure .mk (-1) )

  lazy val context = Context .mk

  lazy val outcome0 : Outcome =
    Outcome_ (
      Seq [Assignment] (
        Assignment .mk (actor0) (resource2) ,
        Assignment .mk (actor1) (resource1) ,
        Assignment .mk (actor2) (resource0)
      )
    )

  lazy val outcome1 : Outcome =
    Outcome_ (
      Seq [Assignment] (
        Assignment .mk (actor0) (resource0) ,
        Assignment .mk (actor1) (resource0) ,
        Assignment .mk (actor2) (resource0)
      )
    )

  lazy val outcome2 : Outcome =
    Outcome_ (
      Seq [Assignment] (
        Assignment .mk (actor0) (resource1) ,
        Assignment .mk (actor1) (resource1) ,
        Assignment .mk (actor2) (resource1)
      )
    )

  lazy val outcome3 : Outcome =
    Outcome_ (
      Seq [Assignment] (
        Assignment .mk (actor0) (resource2) ,
        Assignment .mk (actor1) (resource2) ,
        Assignment .mk (actor2) (resource2)
      )
    )

  lazy val initial0 : TileMessage [Boolean] =
    TileMessageBuilder .mk .build (context) (outcome0) (true)

  lazy val initial1 : TileMessage [Boolean] =
    TileMessageBuilder .mk .build (context) (outcome1) (true)

  lazy val initial2 : TileMessage [Boolean] =
    TileMessageBuilder .mk .build (context) (outcome2) (true)

  lazy val initial3 : TileMessage [Boolean] =
    TileMessageBuilder .mk .build (context) (outcome3) (true)

}

case class ResourceAllocationScenarioExample_ () extends ResourceAllocationScenarioExample

object ResourceAllocationScenarioExample {
  def mk : ResourceAllocationScenarioExample =
    ResourceAllocationScenarioExample_ ()
}


trait ScoringScenarioExample
{



  lazy val actors : Seq [Actor] =
    Seq (
      "Alice", "Benjamin", "Charlotte", "Daniel", "Emily", "Fiona", "George", "Hannah", "Isaac",
      "James", "Kevin", "Lily", "Matthew", "Natalie", "Olivia", "Quinn", "Peter", "Rachel",
      "Sarah", "Timothy", "Ursula", "Victoria", "William", "Xavier", "Yasmine", "Zachary"
    ) .map ( name => Actor .mk (name) )

  private lazy val _resource_zero : Resource = Resource .mk ("0")

  private lazy val _resource_one : Resource = Resource .mk ("1")

  private lazy val _measure_zero : Measure = Measure .mk (0)

  private lazy val _measure_one : Measure = Measure .mk (1)

  lazy val seed_protected_attribute : Long = 127

  lazy val protected_attribute_modulus : Int = 2

  def mod (x : Int) (modulus : Int) : Int =
    ( (x % modulus) + modulus) % modulus

  def as_protected_attribute (x : Int) : Int =
    mod (x) (protected_attribute_modulus)

  lazy val protected_attribute : Seq [Measure] =
    Random .mk .get_next_seq (seed_protected_attribute) (actors .length)
      .map ( x => Measure .mk ( as_protected_attribute (x .intValue) ) )

  lazy val protected_attribute_map : Map [Actor, Measure] =
    actors
      .indices
      .map ( index =>
        Tuple2 [Actor, Measure] (actors .apply (index) , protected_attribute .apply (index) ) )
      .toMap

  def protected_attribute_function (a : Actor) : Measure =
    protected_attribute_map .getOrElse (a , _measure_zero )

  lazy val seed_result : Long = 65535

  lazy val prediction_modulus : Int = 100

  lazy val prediction_limit : Int = prediction_modulus / 2

  def min (a : Int) (b : Int) : Int =
    if ( a < b
    ) a
    else b

  def as_prediction (x : Int) : Int =
    mod (min (x) (prediction_modulus - 1) ) (prediction_modulus)

  def make_binary_resource (x : Int) : Resource =
    if ( x > prediction_limit
    ) _resource_one
    else _resource_zero

  def make_binary_measure (x : Int) : Measure =
    if ( x > prediction_limit
    ) _measure_one
    else _measure_zero

  lazy val result_values : Seq [Int] =
    Random .mk
      .get_next_seq (seed_result) (actors .length)
      .map ( x => as_prediction ( (x .intValue) % prediction_modulus) )

  lazy val prediction_error : Int = 1

  lazy val prediction_bias_on_attribute : Int = 40

  lazy val maximum_acceptable_bias_percentage : Measure = Measure .mk (30)

  lazy val result : Seq [Measure] =
    result_values .map ( x => make_binary_measure (x) )

  lazy val result_map : Map [Actor, Measure] =
    actors
      .indices
      .map ( index =>
        Tuple2 [Actor, Measure] (actors .apply (index) , result .apply (index) ) )
      .toMap

  def result_function (a : Actor) : Measure =
    result_map .getOrElse (a , _measure_zero )

  def add_attribute_bias (index : Int) (original : Int) : Int =
    if ( (protected_attribute .apply (index) == _measure_zero)
    ) original
    else as_prediction (original + prediction_bias_on_attribute)

  def add_prediction_error (value : Int) : Int =
    as_prediction (value + prediction_error)

  lazy val unbiased_prediction : Seq [Resource] =
    result_values
      .map ( x => add_prediction_error (x) )
      .map ( x => make_binary_resource (x) )

  lazy val biased_prediction : Seq [Resource] =
    result_values
      .indices
      .map ( index => add_attribute_bias (index) (result_values .apply (index) ) )
      .map ( x => add_prediction_error (x) )
      .map ( x => make_binary_resource (x) )

  lazy val unbiased_outcome : Outcome =
    Outcome .mk (
      actors
        .indices
        .map ( index =>
          Assignment .mk (actors .apply (index) ) (unbiased_prediction .apply (index) )
        )
    )

  lazy val biased_outcome : Outcome =
    Outcome .mk (
      actors
        .indices
        .map ( index =>
          Assignment .mk (actors .apply (index) ) (biased_prediction .apply (index) )
        )
    )

  def evaluation (resource : Resource) : Measure =
    if ( (resource == _resource_zero)
    ) _measure_zero
    else _measure_one

  lazy val context = Context .mk

  lazy val initial_unbiased : TileMessage [Boolean] =
    TileMessageBuilder .mk .build (context) (unbiased_outcome) (true)

  lazy val initial_biased : TileMessage [Boolean] =
    TileMessageBuilder .mk .build (context) (biased_outcome) (true)

}

case class ScoringScenarioExample_ () extends ScoringScenarioExample

object ScoringScenarioExample {
  def mk : ScoringScenarioExample =
    ScoringScenarioExample_ ()
}


case class UnbiasednessTileSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val ex = ScoringScenarioExample .mk

  lazy val unbiasedness_tile =
    UnbiasednessTile .mk (
      ex .evaluation) (
      ex .result_function) (
      ex .protected_attribute_function) (
      ex .maximum_acceptable_bias_percentage
    )

  def get_coefficient (message : TileMessage [Boolean] ) : TileMessage [Measure] =
    unbiasedness_tile .get_correlation_plumbing (
      unbiasedness_tile .all_actor_triple_tile .apply (message)
    )

  test ("unbiasedness on unbiased sample") (
    check (
      obtained = unbiasedness_tile .apply (ex .initial_unbiased) .contents
    ) (
      expected = true
    )
  )

  test ("unbiasedness on biased sample") (
    check (
      obtained = unbiasedness_tile .apply (ex .initial_biased) .contents
    ) (
      expected = false
    )
  )

  test ("coefficient of unbiased sample") (
    check (
      obtained = get_coefficient (ex .initial_unbiased) .contents
    ) (
      expected = Measure .mk (0)
    )
  )

  test ("coefficient of biased sample") (
    check (
      obtained = get_coefficient (ex .initial_biased) .contents
    ) (
      expected = Measure .mk (42)
    )
  )

}

