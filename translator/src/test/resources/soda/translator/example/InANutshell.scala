package soda.translator.example

case class InANutshell () {

  def f (x: Int ): Int = x + 16
  lazy val b = f (a )
  lazy val a = 1 + 2 + 4 + 8

  def h0 (x: Int, y: Int ): Int = {
    lazy val a = x + y
    lazy val b = x - y

    a * b
  }

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

  def geater_than (a: Int, b: Int ): Boolean =
    a > b

  def max (a: Int, b: Int ): Int =
    if (a > b
    ) a
    else b

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

  case class Example (index: Int ) {
    def min_max (a: Int, b: Int ): MinMaxPair =
      MinMaxPair (min = ConcreteMaxAndMin () .min (a, b ), max = ConcreteMaxAndMin () .max (a, b )
      )
  }

  trait Comparable {
    def is_greater_than (x: Comparable ): Boolean
  }

  case class ComparableMax [T <: Comparable]  () {
    def max (a: T, b: T ): T =
      if (a.is_greater_than (b )
      ) a
      else b
  }

  case class MyClass (instance_parameter: Int ) {
    lazy val class_constant: Int = 1

    def another_function (x: Int ): Int = 2 * x

    case class InnerClass () {
      def main_function (function_parameter: Int ): Int =
        another_function (instance_parameter + class_constant + function_parameter )
    }
  }

  case class TimeOfToday () {
    import java.util.Date

    lazy val get_time: Date = new Date ()
  }

  case class Main () {
    def main (args: Array [String]  ): Unit =
      println ("Hello world!")
  }

  case class PersonName (name: String ) {
    override
    lazy val toString = name
  }
}
