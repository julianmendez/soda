package soda.tiles.fairness.tile

/*
 * This package contains tests for the classes to model a resource allocation scenario.
 */

import   org.scalatest.funsuite.AnyFunSuite
import   soda.tiles.fairness.tool.Actor
import   soda.tiles.fairness.tool.Actor_
import   soda.tiles.fairness.tool.Assignment
import   soda.tiles.fairness.tool.Assignment_
import   soda.tiles.fairness.tool.Context
import   soda.tiles.fairness.tool.Context_
import   soda.tiles.fairness.tool.Measure
import   soda.tiles.fairness.tool.Measure_
import   soda.tiles.fairness.tool.MeasureZero
import   soda.tiles.fairness.tool.MeasureZero_
import   soda.tiles.fairness.tool.Outcome
import   soda.tiles.fairness.tool.Outcome_
import   soda.tiles.fairness.tool.Resource
import   soda.tiles.fairness.tool.Resource_
import   soda.tiles.fairness.tool.TileMessage
import   soda.tiles.fairness.tool.TileMessage_
import   soda.tiles.fairness.tool.TileMessageBuilder
import   soda.tiles.fairness.tool.TileMessageBuilder_
import   soda.tiles.fairness.tool.TilePair
import   soda.tiles.fairness.tool.TilePair_

trait Package

case class EqualityTileSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example = ResourceAllocationScenarioExample_ ()

  lazy val equality_tile = EqualityTile_ (example .measure_sum , example .resource_height)

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

  lazy val example = ResourceAllocationScenarioExample_ ()

  lazy val equity_tile =
    EquityTile_ (example .measure_sum , example .actor_need , example .resource_height)

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

  private def _mk_Assignment (actor : Actor) (resource : Resource) : Assignment =
    Assignment_ (actor, resource)

  def measure_sum (a : Measure) (b : Measure) : Measure =
    Measure_ (a .value + b .value)

  lazy val resource0 = Resource_ ("small box - 0.1 m")

  lazy val resource1 = Resource_ ("medium box - 0.2 m")

  lazy val resource2 = Resource_ ("large box - 0.3 m")

  lazy val actor0 = Actor_ ("Anna A")

  lazy val actor1 = Actor_ ("Bob B")

  lazy val actor2 = Actor_ ("Charlie C")

  lazy val actor_need_map : Map [Actor, Measure] = Seq (
    Tuple2 [Actor, Measure] (actor0 , Measure_ (30) ) ,
    Tuple2 [Actor, Measure] (actor1 , Measure_ (10) ) ,
    Tuple2 [Actor, Measure] (actor2 , Measure_ (0) )
  ) .toMap

  lazy val resource_height_map : Map [Resource, Measure] = Seq (
    Tuple2 [Resource, Measure] (resource0 , Measure_ (10) ) ,
    Tuple2 [Resource, Measure] (resource1 , Measure_ (20) ) ,
    Tuple2 [Resource, Measure] (resource2 , Measure_ (30) )
  ) .toMap

  def actor_need (actor : Actor) : Measure =
    actor_need_map .getOrElse (actor , Measure_ (-1) )

  def resource_height (resource : Resource) : Measure =
    resource_height_map .getOrElse (resource , Measure_ (-1) )

  lazy val context = Context_ ()

  lazy val outcome0 : Outcome =
    Outcome_ (
      Seq [Assignment] (
        _mk_Assignment (actor0) (resource2) ,
        _mk_Assignment (actor1) (resource1) ,
        _mk_Assignment (actor2) (resource0)
      )
    )

  lazy val outcome1 : Outcome =
    Outcome_ (
      Seq [Assignment] (
        _mk_Assignment (actor0) (resource0) ,
        _mk_Assignment (actor1) (resource0) ,
        _mk_Assignment (actor2) (resource0)
      )
    )

  lazy val outcome2 : Outcome =
    Outcome_ (
      Seq [Assignment] (
        _mk_Assignment (actor0) (resource1) ,
        _mk_Assignment (actor1) (resource1) ,
        _mk_Assignment (actor2) (resource1)
      )
    )

  lazy val outcome3 : Outcome =
    Outcome_ (
      Seq [Assignment] (
        _mk_Assignment (actor0) (resource2) ,
        _mk_Assignment (actor1) (resource2) ,
        _mk_Assignment (actor2) (resource2)
      )
    )

  lazy val initial0 : TileMessage [Boolean] =
    TileMessageBuilder_ () .build (context) (outcome0) (true)

  lazy val initial1 : TileMessage [Boolean] =
    TileMessageBuilder_ () .build (context) (outcome1) (true)

  lazy val initial2 : TileMessage [Boolean] =
    TileMessageBuilder_ () .build (context) (outcome2) (true)

  lazy val initial3 : TileMessage [Boolean] =
    TileMessageBuilder_ () .build (context) (outcome3) (true)

}

case class ResourceAllocationScenarioExample_ () extends ResourceAllocationScenarioExample

