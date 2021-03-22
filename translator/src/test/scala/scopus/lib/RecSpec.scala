package scopus.lib

import org.scalatest.funsuite.AnyFunSuite


case class RecSpec() extends AnyFunSuite {

  lazy val exampleSeq: Seq[Int] = Seq(0, 1, 1, 2, 3, 5, 8)

  test("foldLeftWhile with Seq") {
    lazy val initval = Seq()

    def op: (Seq[String], Int) => Seq[String] = (s: Seq[String], e: Int) => s.+:("" + (e + 100))

    def cond: (Seq[String], Int) => Boolean = (s: Seq[String], e: Int) => e < 5

    lazy val expected = Seq("103", "102", "101", "101", "100")
    lazy val obtained = Rec().foldLeftWhile(exampleSeq, Seq(), op, cond)
    assert(obtained == expected)
  }

  test("foldLeft with Seq") {
    lazy val initval = Seq()

    def op: (Seq[String], Int) => Seq[String] = (s: Seq[String], e: Int) => s.+:("" + (e + 100))

    lazy val expected = Seq("108", "105", "103", "102", "101", "101", "100")
    lazy val obtained = Rec().foldLeft(exampleSeq, Seq(), op)
    assert(obtained == expected)
  }

}
