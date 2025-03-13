trait InANutshell
{

  /* (empty) */

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

object InANutshell {
  def mk : InANutshell =
    InANutshell_ ()
}

trait MaxAndMin
{

  /* (empty) */

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

object MaxAndMin {
  def mk : MaxAndMin =
    MaxAndMin_ ()
}

trait Pair [A , B ]
{

  def   fst : A
  def   snd : B

}

case class Pair_ [A, B] (fst : A, snd : B) extends Pair [A, B]

object Pair {
  def mk [A, B] (fst : A) (snd : B) : Pair [A, B] =
    Pair_ [A, B] (fst, snd)
}

trait MinMaxPair
  extends
    Pair [Int, Int]
{

  def   min : Int
  def   max : Int

  lazy val fst : Int = min

  lazy val snd : Int = max

}

case class MinMaxPair_ (min : Int, max : Int) extends MinMaxPair

object MinMaxPair {
  def mk (min : Int) (max : Int) : MinMaxPair =
    MinMaxPair_ (min, max)
}

trait Indexable
{

  def   index : Int

}

case class Indexable_ (index : Int) extends Indexable

object Indexable {
  def mk (index : Int) : Indexable =
    Indexable_ (index)
}

trait Example
  extends Indexable
{

  def   index : Int

  def min_max (a : Int) (b : Int) : MinMaxPair =
    MinMaxPair .mk (min = MaxAndMin .mk .min (a) (b) ) (max = MaxAndMin .mk .max (a) (b) )

}

case class Example_ (index : Int) extends Example

object Example {
  def mk (index : Int) : Example =
    Example_ (index)
}

trait Comparable
{

  def   is_greater_than : Comparable => Boolean

}

case class Comparable_ (is_greater_than : Comparable => Boolean) extends Comparable

object Comparable {
  def mk (is_greater_than : Comparable => Boolean) : Comparable =
    Comparable_ (is_greater_than)
}

trait ComparableMax [A <: Comparable]
{

  /* (empty) */

  def max (a : A) (b : A) : A =
    if ( a .is_greater_than (b)
    ) a
    else b

}

case class ComparableMax_ [A <: Comparable] () extends ComparableMax [A]

object ComparableMax {
  def mk [A <: Comparable] : ComparableMax [A] =
    ComparableMax_ [A] ()
}

trait WithInstance
{

  def   instance_parameter : Int

}

case class WithInstance_ (instance_parameter : Int) extends WithInstance

object WithInstance {
  def mk (instance_parameter : Int) : WithInstance =
    WithInstance_ (instance_parameter)
}

trait MyClass
  extends
    WithInstance
{

  def   instance_parameter : Int

  lazy val class_constant : Int = 1

  def another_function (x : Int) : Int = 2 * x

}

case class MyClass_ (instance_parameter : Int) extends MyClass

object MyClass {
  def mk (instance_parameter : Int) : MyClass =
    MyClass_ (instance_parameter)
}

trait TimeOfToday
{

  /* (empty) */

  import   java.util.Date

  lazy val get_time : Date = new Date ()

}

case class TimeOfToday_ () extends TimeOfToday

object TimeOfToday {
  def mk : TimeOfToday =
    TimeOfToday_ ()
}

trait Main
{

  /* (empty) */

  def main (arguments : Array [String] ) : Unit =
    println ("Hello world!")

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main {
  def mk : Main =
    Main_ ()
}

trait PersonName
{

  def   name : String

  override
  lazy val toString = name

}

case class PersonName_ (name : String) extends PersonName

object PersonName {
  def mk (name : String) : PersonName =
    PersonName_ (name)
}
