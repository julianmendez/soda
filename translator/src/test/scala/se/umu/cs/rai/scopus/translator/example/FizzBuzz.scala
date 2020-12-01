package se.umu.cs.rai.scopus.translator.example

case class FizzBuzz() {

  val Fizz = "Fizz"
  val Buzz = "Buzz"

  def fizzbuzz() = LazyList.from(1).take(100).map(fizzbuzz_term)

  def fizzbuzz_term(n: Int) =
    if ( n % 15 == 0
    ) Fizz + Buzz
    else if ( n % 3 == 0
    ) Fizz
    else if ( n % 5 == 0
    ) Buzz
    else n.toString

}
