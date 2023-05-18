trait FizzBuzz
{

  lazy val fizz = "Fizz"

  lazy val buzz = "Buzz"

  lazy val range = soda.lib.Range_ ()

  lazy val apply : Seq [String] =
    range .apply (100)
      .map ( x => x + 1)
      .map (get_term)

  def get_term (n : Int) : String =
    if ( is_div (n) (15) ) fizz + buzz
    else if ( is_div (n) (3) ) fizz
    else if ( is_div (n) (5) ) buzz
    else n .toString

  def is_div (n : Int) (k : Int) : Boolean =
    n % k == 0

}

case class FizzBuzz_ () extends FizzBuzz
