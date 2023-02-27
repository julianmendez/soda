trait FizzBuzzPatternUnicode
{

  lazy val fizz = "Fizz"

  lazy val buzz = "Buzz"

  private lazy val _range = soda.lib.Range_ ()

  lazy val apply : Seq [String] =
    _range.apply (100)
      .map (  x => x + 1)
      .map (_get_fizz_buzz_term)

  private def _get_fizz_buzz_term (n : Int) : String =
    Tuple2 (n % 3 , n % 5) match  {
      case Tuple2 (0 , 0) => fizz + buzz
      case Tuple2 (0 , x) => fizz
      case Tuple2 (x , 0) => buzz
      case x => n.toString
    }

}

case class FizzBuzzPatternUnicode_ () extends FizzBuzzPatternUnicode
