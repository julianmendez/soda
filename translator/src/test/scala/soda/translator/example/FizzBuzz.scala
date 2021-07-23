package soda.translator.example


trait FizzBuzz {
  import soda.lib.Rec

  lazy val Fizz = "Fizz"
  lazy val Buzz = "Buzz"

  def fizz_buzz () =
    Rec ()
      .range (100 )
      .map (x => x + 1 )
      .map (fizz_buzz_term )

  def fizz_buzz_term (n: Int ) =
    if (n % 15 == 0 ) Fizz + Buzz
    else if (n % 3 == 0 ) Fizz
    else if (n % 5 == 0 ) Buzz
    else n.toString
}

case class FizzBuzzImpl () extends FizzBuzz
