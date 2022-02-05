package soda.example.algorithms

trait FizzBuzzPatternMatching
{

  import   soda.lib.Recursion_

  lazy val fizz = "Fizz"

  lazy val buzz = "Buzz"

  lazy val fizz_buzz =
    Recursion_ ()
      .range (100 )
      .map ((x: Int ) => x + 1 )
      .map (get_fizz_buzz_term )

  def get_fizz_buzz_term (n: Int ) =
    (n % 3, n % 5 ) match  {
      case (0, 0 ) => fizz + buzz
      case (0, x ) => fizz
      case (x, 0 ) => buzz
      case otherwise => n.toString
    }

}

case class FizzBuzzPatternMatching_ () extends FizzBuzzPatternMatching
