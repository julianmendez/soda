package scopus.translator.example


case class FizzBuzz() {

  val Fizz = "Fizz"
  val Buzz = "Buzz"

  def fizz_buzz() = Range(1, 101).map(fizz_buzz_term)

  def fizz_buzz_term(n: Int) =
    if ( n % 15 == 0 ) Fizz + Buzz
    else if ( n % 3 == 0 ) Fizz
    else if ( n % 5 == 0 ) Buzz
    else n.toString

}
