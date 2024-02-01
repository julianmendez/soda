package soda.tiles.fairness.tool

/*
 * This package contains classes to model a fairness scenario.
 */





trait Actor
  extends
    Comparable [Actor]
{

  def   id : String

  def compareTo (other : Actor) : Int =
    id .compareTo (other .id)

  override
  lazy val toString : String =
    id

}

case class Actor_ (id : String) extends Actor

object Actor {
  def mk (id : String) : Actor =
    Actor_ (id)
}

trait Resource
  extends
    Comparable [Resource]
{

  def   id : String

  def compareTo (other : Resource) : Int =
    id .compareTo (other .id)

  override
  lazy val toString : String =
    id

}

case class Resource_ (id : String) extends Resource

object Resource {
  def mk (id : String) : Resource =
    Resource_ (id)
}

trait Measure
  extends
    Comparable [Measure]
{

  def   value : Int

  lazy val minus_infinity = Int .MinValue

  lazy val plus_infinity = Int .MaxValue

  def compareTo (other : Measure) : Int =
    value .compareTo (other .value)

  override
  lazy val toString : String =
    value .toString

}

case class Measure_ (value : Int) extends Measure

object Measure {
  def mk (value : Int) : Measure =
    Measure_ (value)
}

trait Assignment
  extends
    Comparable [Assignment]
{

  def   actor : Actor
  def   resource : Resource

  def compareTo (other : Assignment) : Int =
    if ( actor == other .actor
    ) resource .compareTo (other .resource)
    else actor .compareTo (other .actor)

  override
  lazy val toString : String =
    "\u27E8" + actor + ", " + resource + "\u27E9"

}

case class Assignment_ (actor : Actor, resource : Resource) extends Assignment

object Assignment {
  def mk (actor : Actor) (resource : Resource) : Assignment =
    Assignment_ (actor, resource)
}

trait Outcome
{

  def   assignments : Seq [Assignment]

  override
  lazy val toString : String =
    assignments .mkString (",")

}

case class Outcome_ (assignments : Seq [Assignment]) extends Outcome

object Outcome {
  def mk (assignments : Seq [Assignment]) : Outcome =
    Outcome_ (assignments)
}

trait Context
{



}

case class Context_ () extends Context

object Context {
  def mk : Context =
    Context_ ()
}


/**
 *  r_{x,y} =\frac{\sum _{i=1}^{n}(x_{i} - \bar{x})(y_{i} -
 *  \bar{y})}{\sqrt{\sum _{i=1}^{n}(x_{i} - \bar{x})^2} \sqrt{\sum ^{n} _{i=1}(y_{i} -
 *  \bar{y})^{2}}}
 */

trait MathTool
{



  def squared (x : Double) : Double =
    x * x

  private lazy val _sum_init : Double = 0

  private def _sum_next (accum : Double , elem : Double) : Double =
    accum + elem

  def sum (seq : Seq [Double] ) : Double =
    seq .foldLeft (_sum_init) (_sum_next)

  def average (seq : Seq [Double] ) : Double =
    sum (seq) / seq .length .toDouble

}

case class MathTool_ () extends MathTool

object MathTool {
  def mk : MathTool =
    MathTool_ ()
}

trait Pearson
{

  def   xlist : Seq [Double]
  def   ylist : Seq [Double]

  private lazy val _mt : MathTool = MathTool_ ()

  private def _sum_squared_diff_with (seq : Seq [Double] ) (x_average : Double) : Double =
    _mt .sum (seq .map ( x_i => _mt .squared (x_i - x_average) ) )

  def sum_squared_diff (seq : Seq [Double] ) : Double =
    _sum_squared_diff_with (seq) (_mt .average (seq) )

  private def _sqrt_sum_squared_diff (seq : Seq [Double] ) : Double =
    Math.sqrt (sum_squared_diff (seq) )

  private lazy val _denominator : Double =
    _sqrt_sum_squared_diff (xlist) * _sqrt_sum_squared_diff (ylist)

  private def _multip (x_i : Double) (y_i : Double) (x_average : Double) (y_average : Double) : Double =
    (x_i - x_average) * (y_i - y_average)

  private def _numerator_with (pair_list : Seq [Tuple2 [Double, Double] ] ) (x_average : Double)
      (y_average : Double) : Double =
    _mt .sum (pair_list .map ( pair =>
      _multip (pair ._1) (pair ._2) (x_average) (y_average) ) )

  private lazy val _x_y_together : Seq [Tuple2 [Double, Double] ] =
    xlist .zip (ylist)

  private lazy val _numerator : Double =
    _numerator_with (_x_y_together) (_mt .average (xlist) ) (_mt .average (ylist) )

  lazy val coefficient : Double =
    _numerator / _denominator

}

case class Pearson_ (xlist : Seq [Double], ylist : Seq [Double]) extends Pearson

object Pearson {
  def mk (xlist : Seq [Double]) (ylist : Seq [Double]) : Pearson =
    Pearson_ (xlist, ylist)
}

trait ScoringCategory
{



  lazy val undefined_correlation : Int = 0

  lazy val no_correlation : Int = 1

  lazy val weak_positive_correlation : Int = 2

  lazy val weak_negative_correlation : Int = 3

  lazy val moderate_positive_correlation : Int = 4

  lazy val moderate_negative_correlation : Int = 5

  lazy val strong_positive_correlation : Int = 6

  lazy val strong_negative_correlation : Int = 7

  def categorize (x : Double) : Int =
    if ( (x > 0.5) && (x <= 1.0) ) strong_positive_correlation
    else if ( (x > 0.3) && (x <= 0.5) ) moderate_positive_correlation
    else if ( (x > 0) && (x <= 0.3) ) weak_positive_correlation
    else if ( (x == 0) ) no_correlation
    else if ( (x < 0) && (x >= -0.3) ) weak_negative_correlation
    else if ( (x < -0.3) && (x >= -0.5) ) moderate_negative_correlation
    else if ( (x < -0.5) && (x >= -1.0) ) strong_negative_correlation
    else undefined_correlation

}

case class ScoringCategory_ () extends ScoringCategory

object ScoringCategory {
  def mk : ScoringCategory =
    ScoringCategory_ ()
}


trait TilePair [A , B ]
{

  def   fst : A
  def   snd : B

}

case class TilePair_ [A, B] (fst : A, snd : B) extends TilePair [A, B]

object TilePair {
  def mk [A, B] (fst : A) (snd : B) : TilePair [A, B] =
    TilePair_ [A, B] (fst, snd)
}

trait TileTriple [A , B , C ]
{

  def   fst : A
  def   snd : B
  def   trd : C

}

case class TileTriple_ [A, B, C] (fst : A, snd : B, trd : C) extends TileTriple [A, B, C]

object TileTriple {
  def mk [A, B, C] (fst : A) (snd : B) (trd : C) : TileTriple [A, B, C] =
    TileTriple_ [A, B, C] (fst, snd, trd)
}

trait TileMessage [A ]
{

  def   context : Context
  def   outcome : Outcome
  def   contents : A

}

case class TileMessage_ [A] (context : Context, outcome : Outcome, contents : A) extends TileMessage [A]

object TileMessage {
  def mk [A] (context : Context) (outcome : Outcome) (contents : A) : TileMessage [A] =
    TileMessage_ [A] (context, outcome, contents)
}

trait TileMessageBuilder
{



  def build [A ] (context : Context) (outcome : Outcome) (contents : A) : TileMessage [A] =
    TileMessage_ (context, outcome, contents)

}

case class TileMessageBuilder_ () extends TileMessageBuilder

object TileMessageBuilder {
  def mk : TileMessageBuilder =
    TileMessageBuilder_ ()
}

