package soda.example.forcoq.mathematics

/*
 * This package contains examples of some mathematical functions that can be translated to Coq.
 */

trait Package
trait TriangularNumberForCoq
{

  import   soda.example.forcoq.lib.O_
  import   soda.example.forcoq.lib.S_
  import   soda.example.forcoq.lib.nat

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_number (m : nat) (acc : nat) : nat =
    m match  {
      case S_ (k) => _tailrec_get_number (k) (acc .add ( S_ (k) ) )
      case x => acc
    }

  def get_number (n : nat) : nat =
    _tailrec_get_number (n) ( O_ () )

}

case class TriangularNumberForCoq_ () extends TriangularNumberForCoq
trait FactorialForCoq
{

  import   soda.example.forcoq.lib.O_
  import   soda.example.forcoq.lib.S_
  import   soda.example.forcoq.lib.nat

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (m : nat) (product : nat) : nat =
    m match  {
      case S_ (k) => _tailrec_get_factorial (k) (product .mul ( S_ (k) ) )
      case x => product
    }

  def get_factorial (n : nat) : nat =
    _tailrec_get_factorial (n) (S_ ( O_ () ) )

}

case class FactorialForCoq_ () extends FactorialForCoq
trait FiboExampleInSodaForCoq
{

  import   soda.example.forcoq.lib.O_
  import   soda.example.forcoq.lib.S_
  import   soda.example.forcoq.lib.nat

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fib (m : nat) (a : nat) (b : nat) : nat =
    m match  {
      case S_ (O_ () ) => b
      case S_ (k) => _tailrec_fib (k) (b) (a .add (b) )
      case x => a
    }

  def fib (n : nat) =
    _tailrec_fib (n) (O_ () ) ( S_ (O_ () ) )

}

case class FiboExampleInSodaForCoq_ () extends FiboExampleInSodaForCoq
