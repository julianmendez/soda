package scopus.translator.documentation

import java.util.Date


case class EqualsExample () {
  def f (x: Int,    y: Int) = 2 * x + y

  lazy val answer = f (x=20, y=2)
}

/** Class for a registered person, in snake case */
case class Registered_person (first_name: String, last_name: String) {
  lazy val _separator = " "

  lazy val full_name = first_name + _separator + last_name
}

/** Class for a registered person, in camel case */
case class RegisteredPerson (firstName: String, lastName: String) {
  lazy val _separator = " "

  lazy val fullName = firstName + _separator + lastName
}

trait Agent {
  def identifier: String
}

case class Person (name: String)

case class Agent_Person (name: String) extends Agent {
  lazy val identifier = name
}

trait Ranked_Individual {
  def rank: Int
}

case class Ranked_Agent_Person (name: String, person_rank: Int) extends Agent with Ranked_Individual {
  lazy val identifier = name

  lazy val rank = person_rank
}

trait Element {
  def accept (v: Visitor): Boolean
}

trait Visitor {
  def visit (x: Element): Boolean
}

case class Item (identifier: Int) extends Element {
  def accept (v: Visitor) = v.visit (this)
}

case class Person_Name (name: String) {
  override
  lazy val toString = name
}

case class Group_Name (name: String) {
  private
  lazy val __double = name + " " + name

  lazy val double = __double
}

/**
  * This contains the examples shown in the manual.
  */
case class Manual() {

  lazy val a = 1

  lazy val b: Int = 2

  def plus_one (x: Int): Int = x + 1

  def max (x: Int, y: Int) =
    if ( x > y
    ) x
    else y

  def plus_one (sequence: Seq[Int]) =
    sequence.map(element => element + 1)

  def my_not (x: Boolean) =
    if ( x
    ) false
    else true

  def my_and (x: Boolean, y: Boolean) =
    if ( x
    )
      if ( y
      ) true
      else false
    else false

  def my_or (x: Boolean, y: Boolean) =
    if ( x
    ) true
    else
      if ( y
      ) true
      else false

  def my_xor (x: Boolean, y: Boolean) = (x || y) && ! (x && y)

  lazy val now = new Date()

  def sum (n: Int) = sum_rec (n, 0)

  import scala.annotation.tailrec
        @tailrec
  private
  def sum_rec (n: Int, accum: Int): Int =
    if ( n < 0
    ) accum
    else sum_rec (n - 1, n + accum)

  def sum2 (n: Int) = {
    lazy val result = rec (n, 0)

    import scala.annotation.tailrec
        @tailrec
    def rec (n: Int, accum: Int): Int =
      if ( n < 0
      ) accum
      else rec (n - 1, n + accum)

    result
  }

  def g (x: Int) = x + 1

  def f (x: Int) = {
    lazy val a = g(x)
    lazy val b = g(a)
    a + b
  }

}

trait Abstract_factorial_concise {
  def factorial (n: Int): Int
}

case class Factorial_concise () extends Abstract_factorial_concise {

  def factorial (n: Int) = factorial_rec (n, 1)

  import scala.annotation.tailrec
        @tailrec
  private
  def factorial_rec (n: Int, product: Int): Int =
    if ( n == 0
    ) product
    else factorial_rec (n - 1, n * product)

}

trait Abstract_factorial_verbose {
  def factorial (n : Int) : Int
}

case class Factorial_verbose () extends Abstract_factorial_verbose {

  def factorial (n : Int) = factorial_rec (n, 1)

  import scala.annotation.tailrec
        @tailrec
  private
  def factorial_rec (n : Int, product : Int) : Int =
    if ( n == 0
    ) product
    else factorial_rec (n - 1, n * product)

}

case class Main () {
  def main (args: Array[String]) =
    println ("Hello world!")
}

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

