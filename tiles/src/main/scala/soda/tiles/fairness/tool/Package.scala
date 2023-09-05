package soda.tiles.fairness.tool

/*
 * This package contains classes to model a fairness scenario.
 */

trait Package

trait Actor
  extends
    Comparable [Actor]
{

  def   id : String

  def compareTo (other : Actor) : Int =
    id .compareTo (other .id)

}

case class Actor_ (id : String) extends Actor

trait Resource
  extends
    Comparable [Resource]
{

  def   id : String

  def compareTo (other : Resource) : Int =
    id .compareTo (other .id)

}

case class Resource_ (id : String) extends Resource

trait Measure
  extends
    Comparable [Measure]
{

  def   value : Int

  lazy val minus_infinity = Int .MinValue

  lazy val plus_infinity = Int .MaxValue

  def compareTo (other : Measure) : Int =
    value .compareTo (other .value)

}

case class Measure_ (value : Int) extends Measure

trait MeasureZero
  extends
    Measure
{

  lazy val value : Int = 0

}

case class MeasureZero_ () extends MeasureZero

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

}

case class Assignment_ (actor : Actor, resource : Resource) extends Assignment

trait Outcome
{

  def   assignments : Seq [Assignment]

}

case class Outcome_ (assignments : Seq [Assignment]) extends Outcome

trait Context
{

}

case class Context_ () extends Context


trait TilePair [A , B ]
{

  def   fst : A
  def   snd : B

}

case class TilePair_ [A, B] (fst : A, snd : B) extends TilePair [A, B]

trait TileTriple [A , B , C ]
{

  def   fst : A
  def   snd : B
  def   trd : C

}

case class TileTriple_ [A, B, C] (fst : A, snd : B, trd : C) extends TileTriple [A, B, C]

trait TileMessage [A ]
{

  def   context : Context
  def   outcome : Outcome
  def   contents : A

}

case class TileMessage_ [A] (context : Context, outcome : Outcome, contents : A) extends TileMessage [A]

trait TileMessageBuilder
{

  def build [A ] (context : Context) (outcome : Outcome) (contents : A) : TileMessage [A] =
    TileMessage_ (context, outcome, contents)

}

case class TileMessageBuilder_ () extends TileMessageBuilder

