package soda.translator.documentation

/*
 * This is a Soda tutorial written in Soda.
 * Copyright 2021 Julian Mendez
 * Version: 2021-08-12
 */

trait Shape

trait Movable

trait ShapePainter [A <: Shape]

trait ShapeMover [A <: Shape with Movable]

trait EqualsExample {

  lazy val answer = f (x = 20, y = 2 )

  def f (x: Int, y: Int ) = 2 * x + y
}

trait RegisteredPerson {

  def first_name: String

  def last_name: String

  lazy val _separator = " "

  lazy val full_name = first_name + _separator + last_name
}

case class Person (name: String )

trait Agent {

  def identifier: String
}

case class Agent_ (identifier: String ) extends Agent

trait RankedIndividual {

  def rank: Int
}

case class RankedAgentPerson (identifier: String, rank: Int )  extends Agent with RankedIndividual

trait Element {

  def accept (v: Visitor ): Boolean
}

trait Visitor {

  def visit (x: Element ): Boolean
}

case class Item (identifier: Int ) extends Element {

  def accept (v: Visitor ) = v.visit (this )
}

trait PersonName {

  def name: String

  override
  lazy val toString = name
}

/**
  * This contains the examples shown in the manual.
  */
trait Manual {
  import java.util.Date

  lazy val a = 1

  lazy val b: Int = 2

  lazy val now = new Date ()

  def plus_one (x: Int ): Int = x + 1

  def max (x: Int, y: Int ): Int =
    if (x > y
    ) x
    else y

  def plus_one (sequence: Seq [Int]  ): Seq [Int] =
    sequence.map (element => element + 1 )

  def my_not (x: Boolean ): Boolean =
    if (x
    ) false
    else true

  def my_and (x: Boolean, y: Boolean ): Boolean =
    if (x
    )
      if (y
      ) true
      else false
    else false

  def my_or (x: Boolean, y: Boolean ): Boolean =
    if (x
    ) true
    else
      if (y
      ) true
      else false

  def my_xor (x: Boolean, y: Boolean ) =
    (x || y ) && ! (x && y )

  def sum (n: Int ) =
    {
      lazy val result = rec (n, 0 )

      import scala.annotation.tailrec
        @tailrec
      def rec (n: Int, accum: Int ): Int =
        if (n < 0
        ) accum
        else rec (n - 1, n + accum )

      result }

  def f0 (x: Int ): Int =
    {
      lazy val a = g (x )
      lazy val b = g (a )
      a + b }

  def f1 (x: Int ): Int =
    {
      lazy val result = a + b
      lazy val a = g (x )
      lazy val b = g (a )
      result }

  def g (x: Int ): Int = x + 1
}

trait AbstractFactorialConcise {

  def factorial (n: Int ): Int
}

trait FactorialConcise  extends AbstractFactorialConcise {

  def factorial (n: Int ) =
    {
      lazy val result = rec (n, 1 )

      import scala.annotation.tailrec
        @tailrec
      def rec (n: Int, product: Int ): Int =
        if (n == 0
        ) product
        else rec (n - 1, n * product )

      result }
}

trait AbstractFactorialVerbose {

  def factorial (n: Int ): Int
}

trait FactorialVerbose  extends AbstractFactorialVerbose {

  def factorial (n: Int ) =
    {
      lazy val result = rec (n, 1 )

      import scala.annotation.tailrec
        @tailrec
      def rec (n: Int, product: Int ): Int =
        if (n == 0
        ) product
        else rec (n - 1, n * product )

      result }
}

trait Recursion {

  def fold [A, B, C <: B]  (sequence: Seq [A], initial_value: C, next_value_function: (B, A ) => C, condition: (B, A ) => Boolean  ): C =
    {
      import scala.annotation.tailrec
        @tailrec
      def rec (seq: Seq [A], acc: C ): C =
        if (seq.isEmpty
        ) acc
        else
          if (! condition (acc, seq.head )
          ) acc
          else rec (seq.tail, next_value_function (acc, seq.head )  )

      rec (sequence, initial_value ) }

  def range (length: Int ): Seq [Int] =
    {
      import scala.annotation.tailrec
        @tailrec
      def rec (n: Int, seq: Seq [Int]  ): Seq [Int] =
        if (n <= 0
        ) seq
        else rec (n - 1, seq.+: (n - 1 )  )

      rec (length, Seq [Int]  ()  ) }
}

case class Recursion_ () extends Recursion

trait MainClass {

  def main (arguments: Array [String]  ): Unit =
    println ("Hello world!")
}

case class Main () extends MainClass

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

