trait FizzBuzzPatternMatching
{

  lazy val fizz = "Fizz"

  lazy val buzz = "Buzz"

  lazy val range = soda.lib.Range_ ()

  lazy val apply : Seq [String] =
    range .apply (100)
      .map ( x => x + 1)
      .map (get_term)

  def get_term (n : Int) : String =
    (n % 3 , n % 5) match  {
      case (0 , 0) => fizz + buzz
      case (0 , x) => fizz
      case (x , 0) => buzz
      case otherwise => n .toString
    }

}

case class FizzBuzzPatternMatching_ () extends FizzBuzzPatternMatching
