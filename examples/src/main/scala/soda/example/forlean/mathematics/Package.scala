package soda.example.forlean.mathematics

/*
 * This package contains examples of some mathematical functions that can be translated to Coq.
 */

trait Package

trait FactorialForLean
{

  import   soda.example.forlean.lib.O_
  import   soda.example.forlean.lib.S_
  import   soda.example.forlean.lib.Nat

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (m : Nat) (product : Nat) : Nat =
    m match  {
      case S_ (k) => _tailrec_get_factorial (k) (product .mul ( S_ (k) ) )
      case otherwise => product
    }

  def get_factorial (n : Nat) : Nat =
    _tailrec_get_factorial (n) (S_ ( O_ () ) )

}

case class FactorialForLean_ () extends FactorialForLean


trait FiboExampleInSodaForLean
{

  import   soda.example.forlean.lib.O_
  import   soda.example.forlean.lib.S_
  import   soda.example.forlean.lib.Nat

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fib (m : Nat) (a : Nat) (b : Nat) : Nat =
    m match  {
      case S_ (O_ () ) => b
      case S_ (k) => _tailrec_fib (k) (b) (a .add (b) )
      case otherwise => a
    }

  def fib (n : Nat) =
    _tailrec_fib (n) (O_ () ) ( S_ (O_ () ) )

}

case class FiboExampleInSodaForLean_ () extends FiboExampleInSodaForLean


trait TriangularNumberForLean
{

  import   soda.example.forlean.lib.O_
  import   soda.example.forlean.lib.S_
  import   soda.example.forlean.lib.Nat

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_number (m : Nat) (acc : Nat) : Nat =
    m match  {
      case S_ (k) => _tailrec_get_number (k) (acc .add ( S_ (k) ) )
      case otherwise => acc
    }

  def get_number (n : Nat) : Nat =
    _tailrec_get_number (n) ( O_ () )

}

case class TriangularNumberForLean_ () extends TriangularNumberForLean

