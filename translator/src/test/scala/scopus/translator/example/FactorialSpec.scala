package scopus.translator.example

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

case class FactorialSpec() extends AnyFunSuite {

  val factorialValues = Seq(
    (0, 1), (1, 1), (2, 2), (3, 6), (4, 24), (5, 120), (6, 720), (7, 5040), (8, 40320), (9, 362880), (10, 3628800)
  )

  test("should test the factorial - concise version") {
    val expected = factorialValues
    val obtained = factorialValues
      .map(pair => pair._1)
      .map(n => (n, FactorialConcise().factorial(n)))
    assert(obtained === expected)
  }

  test("should test the factorial - verbose version") {
    val expected = factorialValues
    val obtained = factorialValues
      .map(pair => pair._1)
      .map(n => (n, FactorialVerbose().factorial(n)))
    assert(obtained === expected)
  }

}
