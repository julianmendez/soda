package soda.example.algorithms

trait FizzBuzzPatternMatching
{

  import   soda.lib.Range_

  lazy val fizz = "Fizz"

  lazy val buzz = "Buzz"

  private lazy val _range = Range_ ()

  lazy val fizz_buzz =
    _range.apply (100)
      .map (  (x : Int) => x + 1)
      .map (get_fizz_buzz_term)

  def get_fizz_buzz_term (n : Int) =
    Tuple2 (n % 3, n % 5) match  {
      case Tuple2 (0, 0) => fizz + buzz
      case Tuple2 (0, x) => fizz
      case Tuple2 (x, 0) => buzz
      case x => n.toString
    }

}

case class FizzBuzzPatternMatching_ () extends FizzBuzzPatternMatching
