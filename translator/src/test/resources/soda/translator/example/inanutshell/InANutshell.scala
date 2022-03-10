package soda.example.inanutshell

trait InANutshell
{

  def f (x : Int) : Int = x + 16

  lazy val value = f (numbers)

  lazy val numbers = 1 + 2 + 4 + 8

  def h0 (x : Int) (y : Int) : Int =
    x + y

  def h1 (x : Int) (y : Int) : Int =
    x - y

  def h2 (x : Int) (y : Int) : Int =
    (h0 (x) (y) ) * (h1 (x) (y) )

  def is_greater_than (a : Int) (b : Int) : Boolean =
    a > b

  def max (a : Int) (b : Int) : Int =
    if ( a > b
    ) a
    else b

}

case class InANutshell_ () extends InANutshell

trait MaxAndMin
{

  def max (a : Int) (b : Int) : Int =
    if ( a > b
    ) a
    else b

  def min (a : Int) (b : Int) : Int =
    if ( a < b
    ) a
    else b

}

case class MaxAndMin_ () extends MaxAndMin

trait MinMaxPair
{

  def   min : Int
  def   max : Int

}

case class MinMaxPair_ (min : Int, max : Int) extends MinMaxPair

trait Indexable
{

  def   index : Int

}

case class Indexable_ (index : Int) extends Indexable

trait Example
  extends Indexable
{

  def   index : Int

  def min_max (a : Int) (b : Int) : MinMaxPair =
    MinMaxPair_ (
      min = MaxAndMin_ ().min (a) (b),
      max = MaxAndMin_ ().max (a) (b)
    )

}

case class Example_ (index : Int) extends Example

trait Comparable
{

  def   is_greater_than : Comparable => Boolean

}

case class Comparable_ (is_greater_than : Comparable => Boolean) extends Comparable

trait ComparableMax [A <: Comparable]
{

  def max (a : A) (b : A) : A =
    if ( a.is_greater_than (b)
    ) a
    else b

}

case class ComparableMax_ [A <: Comparable] () extends ComparableMax [A]

trait WithInstance
{

  def   instance_parameter : Int

}

case class WithInstance_ (instance_parameter : Int) extends WithInstance

trait MyClass
  extends
    WithInstance
{

  def   instance_parameter : Int

  lazy val class_constant : Int = 1

  def another_function (x : Int) : Int = 2 * x

}

case class MyClass_ (instance_parameter : Int) extends MyClass

trait TimeOfToday
{

  import   java.util.Date

  lazy val get_time : Date = new Date ()

}

case class TimeOfToday_ () extends TimeOfToday

trait Main
{

  def main (arguments : Array [String] ) : Unit =
    println ("Hello world!")

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

trait PersonName
{

  def   name : String

  override
  lazy val toString = name

}

case class PersonName_ (name : String) extends PersonName
