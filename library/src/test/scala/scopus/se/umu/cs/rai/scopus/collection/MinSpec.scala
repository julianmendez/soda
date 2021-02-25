package scopus.se.umu.cs.rai.scopus.collection

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions

case class MinSpec() extends AnyFunSuite {

  val empty: ESeq[Int] = ESeq[Int]()
  val exampleSeq: Seq[Int] = Seq(0, 1, 1, 2, 3, 5, 8)
  val example: MSeq[Int] = MSeqTranslator().asMSeq(exampleSeq)

  test("prepended") {
    val expected = MSeqTranslator().asMSeq(Seq(1, 0, 1, 1, 2, 3, 5, 8))
    val obtained = Min().prepended(example, 1)
    assert(obtained == expected)
  }

  test("head") {
    val expected = 0
    val obtained = Min().head(example)
    assert(obtained == expected)
  }

  test("tail") {
    val expected = MSeqTranslator().asMSeq(Seq(1, 1, 2, 3, 5, 8))
    val obtained = Min().tail(example)
    assert(obtained == expected)
  }

  //

  test("reverse") {
    val expected = MSeqTranslator().asMSeq(Seq(8, 5, 3, 2, 1, 1, 0))
    val obtained = Min().reverse(example)
    assert(obtained == expected)
  }

  test("length") {
    val expected = 7
    val obtained = Min().length(example)
    assert(obtained == expected)
  }

  test("indexOf something found") {
    val expected = 6
    val obtained = Min().indexOf(example, 8)
    assert(obtained == expected)
  }

  test("indexOf something not found") {
    val expected = -1
    val obtained = Min().indexOf(example, 7)
    assert(obtained == expected)
  }

  test("contains something found") {
    val expected = true
    val obtained = Min().contains(example, 8)
    assert(obtained == expected)
  }

  test("contains something not found") {
    val expected = false
    val obtained = Min().contains(example, 7)
    assert(obtained == expected)
  }

  test("at") {
    val expected = 3
    val obtained = Min().at(example, 4)
    assert(obtained == expected)
  }

  //

  test("take with Seq") {
    val expected0 = Seq(0, 1, 1, 2, 3)
    val obtained0 = exampleSeq.take(5)
    assert(obtained0 == expected0)

    val expected1 = Seq()
    val obtained1 = exampleSeq.take(-100)
    assert(obtained1 == expected1)

    val expected2 = exampleSeq
    val obtained2 = exampleSeq.take(100)
    assert(obtained2 == expected2)
  }

  test("take") {
    val expected0 = MSeqTranslator().asMSeq(Seq(0, 1, 1, 2, 3))
    val obtained0 = Min().take(example, 5)
    assert(obtained0 == expected0)

    val expected1 = empty
    val obtained1 = Min().take(example, -100)
    assert(obtained1 == expected1)

    val expected2 = example
    val obtained2 = Min().take(example, 100)
    assert(obtained2 == expected2)
  }

  test("drop with Seq") {
    val expected0 = Seq(2, 3, 5, 8)
    val obtained0 = exampleSeq.drop(3)
    assert(obtained0 == expected0)

    val expected1 = exampleSeq
    val obtained1 = exampleSeq.drop(-100)
    assert(obtained1 == expected1)

    val expected2 = Seq()
    val obtained2 = exampleSeq.drop(100)
    assert(obtained2 == expected2)
  }

  test("drop") {
    val expected0 = MSeqTranslator().asMSeq(Seq(2, 3, 5, 8))
    val obtained0 = Min().drop(example, 3)
    assert(obtained0 == expected0)

    val expected1 = example
    val obtained1 = Min().drop(example, -100)
    assert(obtained1 == expected1)

    val expected2 = empty
    val obtained2 = Min().drop(example, 100)
    assert(obtained2 == expected2)
  }

  test("takeWhile with Seq") {
    val expected = Seq(0, 1, 1, 2)
    val f: Int => Boolean = e => ! (e == 3)
    val obtained = exampleSeq.takeWhile(f)
    assert(obtained == expected)
  }

  test("takeWhile") {
    val expected = MSeqTranslator().asMSeq(Seq(0, 1, 1, 2))
    val f: Int => Boolean = e => ! (e == 3)
    val obtained = Min().takeWhile(example, f)
    assert(obtained == expected)
  }

  test("dropWhile with Seq") {
    val expected = Seq(3, 5, 8)
    val f: Int => Boolean = e => ! (e == 3)
    val obtained = exampleSeq.dropWhile(f)
    assert(obtained == expected)
  }

  test("dropWhile") {
    val expected = MSeqTranslator().asMSeq(Seq(3, 5, 8))
    val f: Int => Boolean = e => ! (e == 3)
    val obtained = Min().dropWhile(example, f)
    assert(obtained == expected)
  }

  test("splitAt with Seq") {
    val expected = (Seq(0, 1, 1), Seq(2, 3, 5, 8))
    val obtained = exampleSeq.splitAt(3)
    assert(obtained == expected)
  }

  test("splitAt") {
    val expected = (MSeqTranslator().asMSeq(Seq(0, 1, 1)), MSeqTranslator().asMSeq(Seq(2, 3, 5, 8)))
    val obtained = Min().splitAt(example, 3)
    assert(obtained == expected)
  }

  test("span with Seq") {
    val expected = (Seq(0, 1, 1, 2, 3), Seq(5, 8))
    val obtained = exampleSeq.span(x => ! (x == 5))
    assert(obtained == expected)
  }

  test("span") {
    val expected = (MSeqTranslator().asMSeq(Seq(0, 1, 1, 2, 3)), MSeqTranslator().asMSeq(Seq(5, 8)))
    val obtained = Min().span(example, (x: Int) => ! (x == 5))
    assert(obtained == expected)
  }

  //

  test("append") {
    val expected = MSeqTranslator().asMSeq(Seq(0, 1, 1, 2, 3, 5, 8, 13))
    val obtained = Min().appended(example, 13)
    assert(obtained == expected)
  }

  test("last") {
    val expected = 8
    val obtained = Min().last(example)
    assert(obtained == expected)
  }

  test("concat") {
    val expected = MSeqTranslator().asMSeq(Seq(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
    val second = MSeqTranslator().asMSeq(Seq(13, 21, 34, 55))
    val obtained = Min().concat(example, second)
    assert(obtained == expected)
  }

  test("slice with Seq") {
    val expected0 = Seq(1, 2, 3)
    val obtained0 = exampleSeq.slice(2, 5)
    assert(obtained0 == expected0)

    val expected1 = Seq()
    val obtained1 = exampleSeq.slice(5, 2)
    assert(obtained1 == expected1)

    val expected2 = Seq()
    val obtained2 = exampleSeq.slice(3, 3)
    assert(obtained2 == expected2)

    val expected3 = Seq()
    val obtained3 = exampleSeq.slice(100, 5)
    assert(obtained3 == expected3)

    val expected4 = Seq()
    val obtained4 = exampleSeq.slice(100, 200)
    assert(obtained4 == expected4)

    val expected5 = exampleSeq
    val obtained5 = exampleSeq.slice(-100, 200)
    assert(obtained5 == expected5)
  }

  test("slice") {
    val expected0 = MSeqTranslator().asMSeq(Seq(1, 2, 3))
    val obtained0 = Min().slice(example, 2, 5)
    assert(obtained0 == expected0)

    val expected1 = empty
    val obtained1 = Min().slice(example, 5, 2)
    assert(obtained1 == expected1)

    val expected2 = empty
    val obtained2 = Min().slice(example, 3, 3)
    assert(obtained2 == expected2)

    val expected3 = empty
    val obtained3 = Min().slice(example, 100, 5)
    assert(obtained3 == expected3)

    val expected4 = empty
    val obtained4 = Min().slice(example, 100, 200)
    assert(obtained4 == expected4)

    val expected5 = example
    val obtained5 = Min().slice(example, -100, 200)
    assert(obtained5 == expected5)
  }

  //

  test("forall with Seq") {
    val expected0 = true
    val pred0: Int => Boolean = x => ! (x == 7)
    val obtained0 = exampleSeq.forall(pred0)
    assert(obtained0 == expected0)

    val expected1 = false
    val pred1: Int => Boolean = x => x < 7
    val obtained1 = exampleSeq.forall(pred1)
    assert(obtained1 == expected1)

    val expected2 = true
    val pred2: Int => Boolean = x => x == 7
    val obtained2 = Seq().forall(pred2)
    assert(obtained2 == expected2)
  }

  test("forall") {
    val expected0 = true
    val pred0: Int => Boolean = x => ! (x == 7)
    val obtained0 = Min().forall(example, pred0)
    assert(obtained0 == expected0)

    val expected1 = false
    val pred1: Int => Boolean = x => x < 7
    val obtained1 = Min().forall(example, pred1)
    assert(obtained1 == expected1)

    val expected2 = true
    val pred2: Int => Boolean = x => x == 7
    val obtained2 = Min().forall(empty, pred2)
    assert(obtained2 == expected2)
  }

  test("exists with Seq") {
    val expected0 = true
    val pred0: Int => Boolean = x => x == 8
    val obtained0 = exampleSeq.exists(pred0)
    assert(obtained0 == expected0)

    val expected1 = false
    val pred1: Int => Boolean = x => x == 7
    val obtained1 = exampleSeq.exists(pred1)
    assert(obtained1 == expected1)

    val expected2 = false
    val pred2: Int => Boolean = x => x == 7
    val obtained2 = Seq().exists(pred2)
    assert(obtained2 == expected2)
  }

  test("exists") {
    val expected0 = true
    val pred0: Int => Boolean = x => x == 8
    val obtained0 = Min().exists(example, pred0)
    assert(obtained0 == expected0)

    val expected1 = false
    val pred1: Int => Boolean = x => x == 7
    val obtained1 = Min().exists(example, pred1)
    assert(obtained1 == expected1)

    val expected2 = false
    val pred2: Int => Boolean = x => x == 7
    val obtained2 = Min().exists(empty, pred2)
    assert(obtained2 == expected2)
  }

  test("find with Seq") {
    val expected0 = Some(0)
    val pred0: Int => Boolean = x => ! (x == 7)
    val obtained0 = exampleSeq.find(pred0)
    assert(obtained0 == expected0)

    val expected1 = Some(8)
    val pred1: Int => Boolean = x => x == 8
    val obtained1 = exampleSeq.find(pred1)
    assert(obtained1 == expected1)

    val expected2 = None
    val pred2: Int => Boolean = x => x == 7
    val obtained2 = Seq().find(pred2)
    assert(obtained2 == expected2)
  }

  test("find") {
    val expected0 = Some(0)
    val pred0: Int => Boolean = x => ! (x == 7)
    val obtained0 = Min().find(example, pred0)
    assert(obtained0 == expected0)

    val expected1 = Some(8)
    val pred1: Int => Boolean = x => x == 8
    val obtained1 = Min().find(example, pred1)
    assert(obtained1 == expected1)

    val expected2 = None
    val pred2: Int => Boolean = x => x == 7
    val obtained2 = Min().find(empty, pred2)
    assert(obtained2 == expected2)
  }

  test("filter with Seq") {
    val expected = Seq(0, 3)
    val pred: Int => Boolean = x => x % 3 == 0
    val obtained = exampleSeq.filter(pred)
    assert(obtained == expected)
  }

  test("filter") {
    val expected = MSeqTranslator().asMSeq(Seq(0, 3))
    val f: Int => Boolean = x => x % 3 == 0
    val obtained = Min().filter(example, f)
    assert(obtained == expected)
  }

  test("map with Seq") {
    val expected = Seq(100, 101, 101, 102, 103, 105, 108)
    val f: Int => Int = x => x + 100
    val obtained = exampleSeq.map(f)
    assert(obtained == expected)
  }

  test("map in the same type") {
    val expected = MSeqTranslator().asMSeq(Seq(100, 101, 101, 102, 103, 105, 108))
    val f: Int => Int = x => x + 100
    val obtained = Min().map0(example, f)
    assert(obtained == expected)
  }

  test("foldLeft with Seq") {
    val expected = Seq(108, 105, 103, 102, 101, 101, 100)
    val obtained = exampleSeq.foldLeft(Seq[Int]())((s: Seq[Int], e: Int) => s.+:((e + 100)))
    assert(obtained == expected)
  }

  test("foldLeft in the same type") {
    val expected = MSeqTranslator().asMSeq(Seq(108, 105, 103, 102, 101, 101, 100))
    val obtained = Min().foldLeft0(example)(Min().empty,
      (s: MSeq[Int], e: Int) => Min().prepended(s, e + 100)
    )
    assert(obtained == expected)
  }

}
