trait FizzBuzzPatternUnicode
{



  lazy val fizz = "Fizz"

  lazy val buzz = "Buzz"

  lazy val range = soda.lib.Range_ ()

  def get_term (n : Int) : String =
    Tuple2 (n % 3 , n % 5) match  {
      case Tuple2 (0 , 0) => fizz + buzz
      case Tuple2 (0 , x) => fizz
      case Tuple2 (x , 0) => buzz
      case _otherwise => n .toString
    }

  lazy val apply : Seq [String] =
    range.apply (100)
      .map ( x => x + 1)
      .map (get_term)

}

case class FizzBuzzPatternUnicode_ () extends FizzBuzzPatternUnicode

object FizzBuzzPatternUnicode {
  def mk : FizzBuzzPatternUnicode =
    FizzBuzzPatternUnicode_ ()
}
