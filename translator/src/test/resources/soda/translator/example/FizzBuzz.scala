package soda.example.algorithms

trait FizzBuzz
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
    if (is_divisible_by (n, 15 ) ) fizz + buzz
    else if (is_divisible_by (n, 3 ) ) fizz
    else if (is_divisible_by (n, 5 ) ) buzz
    else n.toString

  def is_divisible_by (n: Int, k: Int ): Boolean =
    n % k == 0

}

case class FizzBuzz_ ()
  extends
    FizzBuzz
{

}
