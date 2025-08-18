package soda.example.forrocq.mathematics

/*
 * This package contains examples of some mathematical functions that can be translated to Rocq.
 */

import   soda.example.forrocq.lib.O_
import   soda.example.forrocq.lib.S_
import   soda.example.forrocq.lib.nat





trait FactorialForRocq
{



  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (m : nat) (product : nat) : nat =
    m match  {
      case S_ (k) => _tailrec_get_factorial (k) (product .mul (S_ (k) ) )
      case _otherwise => product
    }

  def get_factorial (n : nat) : nat =
    _tailrec_get_factorial (n) (S_ (O_ () ) )

}

case class FactorialForRocq_ () extends FactorialForRocq

object FactorialForRocq {
  def mk : FactorialForRocq =
    FactorialForRocq_ ()
}


trait FiboExampleInSodaForRocq
{



  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fib (m : nat) (a : nat) (b : nat) : nat =
    m match  {
      case S_ (O_ () ) => b
      case S_ (k) => _tailrec_fib (k) (b) (a .add (b) )
      case _otherwise => a
    }

  def fib (n : nat) : nat =
    _tailrec_fib (n) (O_ () ) (S_ (O_ () ) )

}

case class FiboExampleInSodaForRocq_ () extends FiboExampleInSodaForRocq

object FiboExampleInSodaForRocq {
  def mk : FiboExampleInSodaForRocq =
    FiboExampleInSodaForRocq_ ()
}


trait TriangularNumberForRocq
{



  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_number (m : nat) (acc : nat) : nat =
    m match  {
      case S_ (k) => _tailrec_get_number (k) (acc .add (S_ (k) ) )
      case _otherwise => acc
    }

  def get_number (n : nat) : nat =
    _tailrec_get_number (n) (O_ () )

}

case class TriangularNumberForRocq_ () extends TriangularNumberForRocq

object TriangularNumberForRocq {
  def mk : TriangularNumberForRocq =
    TriangularNumberForRocq_ ()
}

