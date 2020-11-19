package se.umu.cs.rai.scopus.translator.documentation

import java.util.Date

import scala.annotation.tailrec

/** Class for a registered person, in snake case */
case class Registered_person (first_name: String, last_name: String) {
  val _separator = " "

  val full_name = first_name + _separator + last_name
}

/** Class for a registered person, in camel case */
case class RegisteredPerson (firstName: String, lastName: String) {
  val _separator = " "

  val fullName = firstName + _separator + lastName
}

trait Agent {
  val identifier: String
}

case class Person (name: String)

case class Agent_Person (name: String) extends Agent {
  val identifier = name
}

trait Ranked_Individual {
  val rank: Int
}

case class Ranked_Agent_Person (name: String, person_rank: Int) extends Agent with Ranked_Individual {
  val identifier = name

  val rank = person_rank
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
  val toString = name
}

/**
  * This contains the examples shown in the manual.
  */
case class Manual() {

  val a = 1

  val b: Int = 2

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

  val now = new Date()

  def sum (n: Int) = sum_rec (n, 0)

  @tailrec final
  def sum_rec (n: Int, accum: Int): Int =
    if ( n < 0
    ) accum
    else sum_rec (n - 1, n + accum)

}

trait AbstractFactorialConcise {
  def factorial (n: Int): Int
}

case class FactorialConcise () extends AbstractFactorialConcise {

  def factorial (n: Int) = factorial_rec (n, 1)

  @tailrec final
  def factorial_rec (n: Int, product: Int): Int =
    if ( n == 0
    ) product
    else factorial_rec (n - 1, n * product)

}

trait AbstractFactorialVerbose {
  def factorial (n : Int) : Int
}

case class FactorialVerbose () extends AbstractFactorialVerbose {

  def factorial (n : Int) = factorial_rec (n, 1)

  @tailrec final
  def factorial_rec (n : Int, product : Int) : Int =
    if ( n == 0
    ) product
    else factorial_rec (n - 1, n * product)

}
