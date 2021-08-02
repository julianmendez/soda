package soda.translator.example


trait FizzBuzz {
  import soda.lib.Rec

  lazy val fizz = "Fizz"
  lazy val buzz = "Buzz"

  def fizz_buzz () =
    Rec ()
      .range (100 )
      .map (x => x + 1 )
      .map (fizz_buzz_term )

  def fizz_buzz_term (n: Int ) =
    if (is_divisible_by (n, 15 ) ) fizz + buzz
    else if (is_divisible_by (n, 3 ) ) fizz
    else if (is_divisible_by (n, 5 ) ) buzz
    else n.toString

  def is_divisible_by (n: Int, k: Int ): Boolean =
    n % k == 0
}

case class FizzBuzzImpl () extends FizzBuzz
