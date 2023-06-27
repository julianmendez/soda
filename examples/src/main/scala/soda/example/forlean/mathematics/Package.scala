package soda.example.forlean.mathematics

/*
 * This package contains examples of some mathematical functions that can be translated to Coq.
 */

import   soda.example.forlean.lib.Zero_
import   soda.example.forlean.lib.Succ_
import   soda.example.forlean.lib.Nat

trait Package

trait FactorialForLean
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (m : Nat) (product : Nat) : Nat =
    m match  {
      case Succ_ (k) => _tailrec_get_factorial (k) (product .mul (Succ_ (k) ) )
      case otherwise => product
    }

  def get_factorial (n : Nat) : Nat =
    _tailrec_get_factorial (n) (Succ_ ( Zero_ () ) )

}

case class FactorialForLean_ () extends FactorialForLean


trait FiboExampleInSodaForLean
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fib (m : Nat) (a : Nat) (b : Nat) : Nat =
    m match  {
      case Succ_ (Zero_ () ) => b
      case Succ_ (k) => _tailrec_fib (k) (b) (a .add (b) )
      case otherwise => a
    }

  def fib (n : Nat) =
    _tailrec_fib (n) (Zero_ () ) (Succ_ (Zero_ () ) )

}

case class FiboExampleInSodaForLean_ () extends FiboExampleInSodaForLean


trait TriangularNumberForLean
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_number (m : Nat) (acc : Nat) : Nat =
    m match  {
      case Succ_ (k) => _tailrec_get_number (k) (acc .add (Succ_ (k) ) )
      case otherwise => acc
    }

  def get_number (n : Nat) : Nat =
    _tailrec_get_number (n) (Zero_ () )

}

case class TriangularNumberForLean_ () extends TriangularNumberForLean

