package scopus.translator.documentation

import java.util.Date


case class EqualsExample (  ) {
  def f ( x: Int ,    y: Int ) = 2 * x + y

  lazy val answer = f ( x=20 , y=2 )
}

/** Class for a registered person, in snake case */
case class Registered_person ( first_name: String , last_name: String ) {
  lazy val _separator = " "

  lazy val full_name = first_name + _separator + last_name
}

/** Class for a registered person, in camel case */
case class RegisteredPerson ( firstName: String , lastName: String ) {
  lazy val _separator = " "

  lazy val fullName = firstName + _separator + lastName
}

trait Agent {
  def identifier: String
}

case class Person ( name: String )

case class AgentPerson ( name: String ) extends Agent {
  lazy val identifier = name
}

trait RankedIndividual {
  def rank: Int
}

case class RankedAgentPerson ( name: String , person_rank: Int ) extends Agent with RankedIndividual {
  lazy val identifier = name

  lazy val rank = person_rank
}

trait Element {
  def accept ( v: Visitor ) : Boolean
}

trait Visitor {
  def visit ( x: Element ) : Boolean
}

case class Item ( identifier: Int ) extends Element {
  def accept ( v: Visitor ) = v.visit ( this )
}

case class PersonName ( name: String ) {
  override
  lazy val toString = name
}

/**
  * This contains the examples shown in the manual.
  */
case class Manual (  ) {

  lazy val a = 1

  lazy val b: Int = 2

  def plus_one ( x: Int ) : Int = x + 1

  def max ( x: Int , y: Int ) =
    if ( x > y
    ) x
    else y

  def plus_one ( sequence: Seq [ Int ]  ) =
    sequence.map ( element => element + 1 )

  def my_not ( x: Boolean ) =
    if ( x
    ) false
    else true

  def my_and ( x: Boolean , y: Boolean ) =
    if ( x
    )
      if ( y
      ) true
      else false
    else false

  def my_or ( x: Boolean , y: Boolean ) =
    if ( x
    ) true
    else
      if ( y
      ) true
      else false

  def my_xor ( x: Boolean , y: Boolean ) = ( x || y ) && ! ( x && y )

  lazy val now = new Date (  )

  def sum ( n: Int ) = {
    lazy val result = rec ( n , 0 )

    import scala.annotation.tailrec
        @tailrec
    def rec ( n: Int , accum: Int ) : Int =
      if ( n < 0
      ) accum
      else rec ( n - 1 , n + accum )

    result
  }

  def g ( x: Int ) = x + 1

  def f ( x: Int ) = {
    lazy val a = g ( x )
    lazy val b = g ( a )
    a + b
  }

}

trait AbstractFactorialConcise {
  def factorial ( n: Int ) : Int
}

case class FactorialConcise (  ) extends AbstractFactorialConcise {

  def factorial ( n: Int ) = {
    lazy val result = rec ( n , 1 )

    import scala.annotation.tailrec
        @tailrec
    def rec ( n: Int , product: Int ) : Int =
      if ( n == 0
      ) product
      else rec ( n - 1 , n * product )

    result
  }
}

trait AbstractFactorialVerbose {
  def factorial ( n: Int ) : Int
}

case class FactorialVerbose (  ) extends AbstractFactorialVerbose {

  def factorial ( n: Int ) = {
    lazy val result = rec ( n , 1 )

    import scala.annotation.tailrec
        @tailrec
    def rec ( n: Int , product: Int ) : Int =
      if ( n == 0
      ) product
      else rec ( n - 1 , n * product )

    result
  }
}

case class Main (  ) {
  def main ( args: Array [ String ]  ) =
    println ("Hello world!")
}

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

