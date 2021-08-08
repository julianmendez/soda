package soda.translator.example.inanutshell

trait InANutshell {

  def f (x: Int ): Int = x + 16
  lazy val b = f (a )
  lazy val a = 1 + 2 + 4 + 8

  def h0 (x: Int, y: Int ): Int =
    {
      lazy val a = x + y
      lazy val b = x - y
      a * b }

  def h1 (x: Int, y: Int ): Int =
    {
      lazy val a = x + y
      lazy val b = x - y
      a * b }

  def h2 (x: Int, y: Int ): Int =
    {
      lazy val result = a * b
      lazy val a = x + y
      lazy val b = x - y
      result }

  def is_greater_than (a: Int, b: Int ): Boolean =
    a > b

  def max (a: Int, b: Int ): Int =
    if (a > b
    ) a
    else b
}

trait MaxAndMin {
  def max (a: Int, b: Int ): Int =
    if (a > b
    ) a
    else b

  def min (a: Int, b: Int ): Int =
    if (a < b
    ) a
    else b
}

case class ConcreteMaxAndMin () extends MaxAndMin

case class MinMaxPair (min: Int, max: Int )

trait Example {

  def index: Int

  def min_max (a: Int, b: Int ): MinMaxPair =
    MinMaxPair (min = ConcreteMaxAndMin () .min (a, b ), max = ConcreteMaxAndMin () .max (a, b )    )
}

case class Example_ (index: Int ) extends Example

trait Comparable {
  def is_greater_than (x: Comparable ): Boolean
}

trait ComparableMax [T <: Comparable] {
  def max (a: T, b: T ): T =
    if (a.is_greater_than (b )
    ) a
    else b
}

trait MyClass {

  def instance_parameter: Int

  lazy val class_constant: Int = 1

  def another_function (x: Int ): Int = 2 * x
}

trait TimeOfToday {
  import java.util.Date

  lazy val get_time: Date = new Date ()
}

trait MainClass {
  def main (args: Array [String]  ): Unit =
    println ("Hello world!")
}

case class Main () extends MainClass

trait PersonName {

  def name: String

  override
  lazy val toString = name
}
