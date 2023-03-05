trait FizzBuzz
{

  lazy val fizz = "Fizz"

  lazy val buzz = "Buzz"

  private lazy val _range = soda.lib.Range_ ()

  lazy val apply : Seq [String] =
    _range.apply (100)
      .map ( (x : Int) => x + 1)
      .map (_get_fizz_buzz_term)

  private def _get_fizz_buzz_term (n : Int) : String =
    if ( _is_divisible_by (n) (15) ) fizz + buzz
    else if ( _is_divisible_by (n) (3) ) fizz
    else if ( _is_divisible_by (n) (5) ) buzz
    else n.toString

  private def _is_divisible_by (n : Int) (k : Int) : Boolean =
    n % k == 0

}

case class FizzBuzz_ () extends FizzBuzz
