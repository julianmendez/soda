package soda.collection

import org.scalatest.funsuite.AnyFunSuite

case class MinSpec () extends AnyFunSuite {

  lazy val empty: ESeq [Int] = ESeq [Int]  ()
  lazy val exampleSeq: Seq [Int] = Seq (0, 1, 1, 2, 3, 5, 8 )
  lazy val example: MSeq [Int] = MSeqTranslator () .asMSeq (exampleSeq )

  test ("prepended") {
    lazy val expected = MSeqTranslator () .asMSeq (Seq (1, 0, 1, 1, 2, 3, 5, 8 )  )
    lazy val obtained = Min () .prepended (example, 1 )
    assert (obtained == expected )
  }

  test ("head") {
    lazy val expected = 0
    lazy val obtained = Min () .head (example )
    assert (obtained == expected )
  }

  test ("tail") {
    lazy val expected = MSeqTranslator () .asMSeq (Seq (1, 1, 2, 3, 5, 8 )  )
    lazy val obtained = Min () .tail (example )
    assert (obtained == expected )
  }

  //

  test ("reverse") {
    lazy val expected = MSeqTranslator () .asMSeq (Seq (8, 5, 3, 2, 1, 1, 0 )  )
    lazy val obtained = Min () .reverse (example )
    assert (obtained == expected )
  }

  test ("length") {
    lazy val expected = 7
    lazy val obtained = Min () .length (example )
    assert (obtained == expected )
  }

  test ("indexOf something found") {
    lazy val expected = 6
    lazy val obtained = Min () .indexOf (example, 8 )
    assert (obtained == expected )
  }

  test ("indexOf something not found") {
    lazy val expected = -1
    lazy val obtained = Min () .indexOf (example, 7 )
    assert (obtained == expected )
  }

  test ("contains something found") {
    lazy val expected = true
    lazy val obtained = Min () .contains (example, 8 )
    assert (obtained == expected )
  }

  test ("contains something not found") {
    lazy val expected = false
    lazy val obtained = Min () .contains (example, 7 )
    assert (obtained == expected )
  }

  test ("at") {
    lazy val expected = Some (3 )
    lazy val obtained = Min () .at (example, 4 )
    assert (obtained == expected )
  }

  //

  test ("take with Seq") {
    lazy val expected0 = Seq (0, 1, 1, 2, 3 )
    lazy val obtained0 = exampleSeq.take (5 )
    assert (obtained0 == expected0 )

    lazy val expected1 = Seq ()
    lazy val obtained1 = exampleSeq.take (-100 )
    assert (obtained1 == expected1 )

    lazy val expected2 = exampleSeq
    lazy val obtained2 = exampleSeq.take (100 )
    assert (obtained2 == expected2 )
  }

  test ("take") {
    lazy val expected0 = MSeqTranslator () .asMSeq (Seq (0, 1, 1, 2, 3 )  )
    lazy val obtained0 = Min () .take (example, 5 )
    assert (obtained0 == expected0 )

    lazy val expected1 = empty
    lazy val obtained1 = Min () .take (example, -100 )
    assert (obtained1 == expected1 )

    lazy val expected2 = example
    lazy val obtained2 = Min () .take (example, 100 )
    assert (obtained2 == expected2 )
  }

  test ("drop with Seq") {
    lazy val expected0 = Seq (2, 3, 5, 8 )
    lazy val obtained0 = exampleSeq.drop (3 )
    assert (obtained0 == expected0 )

    lazy val expected1 = exampleSeq
    lazy val obtained1 = exampleSeq.drop (-100 )
    assert (obtained1 == expected1 )

    lazy val expected2 = Seq ()
    lazy val obtained2 = exampleSeq.drop (100 )
    assert (obtained2 == expected2 )
  }

  test ("drop") {
    lazy val expected0 = MSeqTranslator () .asMSeq (Seq (2, 3, 5, 8 )  )
    lazy val obtained0 = Min () .drop (example, 3 )
    assert (obtained0 == expected0 )

    lazy val expected1 = example
    lazy val obtained1 = Min () .drop (example, -100 )
    assert (obtained1 == expected1 )

    lazy val expected2 = empty
    lazy val obtained2 = Min () .drop (example, 100 )
    assert (obtained2 == expected2 )
  }

  test ("takeWhile with Seq") {
    lazy val expected = Seq (0, 1, 1, 2 )
    lazy val f: Int => Boolean = e => ! (e == 3 )
    lazy val obtained = exampleSeq.takeWhile (f )
    assert (obtained == expected )
  }

  test ("takeWhile") {
    lazy val expected = MSeqTranslator () .asMSeq (Seq (0, 1, 1, 2 )  )
    lazy val f: Int => Boolean = e => ! (e == 3 )
    lazy val obtained = Min () .takeWhile (example, f )
    assert (obtained == expected )
  }

  test ("dropWhile with Seq") {
    lazy val expected = Seq (3, 5, 8 )
    lazy val f: Int => Boolean = e => ! (e == 3 )
    lazy val obtained = exampleSeq.dropWhile (f )
    assert (obtained == expected )
  }

  test ("dropWhile") {
    lazy val expected = MSeqTranslator () .asMSeq (Seq (3, 5, 8 )  )
    lazy val f: Int => Boolean = e => ! (e == 3 )
    lazy val obtained = Min () .dropWhile (example, f )
    assert (obtained == expected )
  }

  test ("splitAt with Seq") {
    lazy val expected = (Seq (0, 1, 1 ), Seq (2, 3, 5, 8 )  )
    lazy val obtained = exampleSeq.splitAt (3 )
    assert (obtained == expected )
  }

  test ("splitAt") {
    lazy val expected = (MSeqTranslator () .asMSeq (Seq (0, 1, 1 )  ), MSeqTranslator () .asMSeq (Seq (2, 3, 5, 8 )  )  )
    lazy val obtained = Min () .splitAt (example, 3 )
    assert (obtained == expected )
  }

  test ("span with Seq") {
    lazy val expected = (Seq (0, 1, 1, 2, 3 ), Seq (5, 8 )  )
    lazy val obtained = exampleSeq.span (x => ! (x == 5 )  )
    assert (obtained == expected )
  }

  test ("span") {
    lazy val expected = (MSeqTranslator () .asMSeq (Seq (0, 1, 1, 2, 3 )  ), MSeqTranslator () .asMSeq (Seq (5, 8 )  )  )
    lazy val obtained = Min () .span (example, (x: Int ) => ! (x == 5 )  )
    assert (obtained == expected )
  }

  //

  test ("append") {
    lazy val expected = MSeqTranslator () .asMSeq (Seq (0, 1, 1, 2, 3, 5, 8, 13 )  )
    lazy val obtained = Min () .appended (example, 13 )
    assert (obtained == expected )
  }

  test ("last") {
    lazy val expected = 8
    lazy val obtained = Min () .last (example )
    assert (obtained == expected )
  }

  test ("concat") {
    lazy val expected = MSeqTranslator () .asMSeq (Seq (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55 )  )
    lazy val second = MSeqTranslator () .asMSeq (Seq (13, 21, 34, 55 )  )
    lazy val obtained = Min () .concat (example, second )
    assert (obtained == expected )
  }

  test ("slice with Seq") {
    lazy val expected0 = Seq (1, 2, 3 )
    lazy val obtained0 = exampleSeq.slice (2, 5 )
    assert (obtained0 == expected0 )

    lazy val expected1 = Seq ()
    lazy val obtained1 = exampleSeq.slice (5, 2 )
    assert (obtained1 == expected1 )

    lazy val expected2 = Seq ()
    lazy val obtained2 = exampleSeq.slice (3, 3 )
    assert (obtained2 == expected2 )

    lazy val expected3 = Seq ()
    lazy val obtained3 = exampleSeq.slice (100, 5 )
    assert (obtained3 == expected3 )

    lazy val expected4 = Seq ()
    lazy val obtained4 = exampleSeq.slice (100, 200 )
    assert (obtained4 == expected4 )

    lazy val expected5 = exampleSeq
    lazy val obtained5 = exampleSeq.slice (-100, 200 )
    assert (obtained5 == expected5 )
  }

  test ("slice") {
    lazy val expected0 = MSeqTranslator () .asMSeq (Seq (1, 2, 3 )  )
    lazy val obtained0 = Min () .slice (example, 2, 5 )
    assert (obtained0 == expected0 )

    lazy val expected1 = empty
    lazy val obtained1 = Min () .slice (example, 5, 2 )
    assert (obtained1 == expected1 )

    lazy val expected2 = empty
    lazy val obtained2 = Min () .slice (example, 3, 3 )
    assert (obtained2 == expected2 )

    lazy val expected3 = empty
    lazy val obtained3 = Min () .slice (example, 100, 5 )
    assert (obtained3 == expected3 )

    lazy val expected4 = empty
    lazy val obtained4 = Min () .slice (example, 100, 200 )
    assert (obtained4 == expected4 )

    lazy val expected5 = example
    lazy val obtained5 = Min () .slice (example, -100, 200 )
    assert (obtained5 == expected5 )
  }

  //

  test ("forall with Seq") {
    lazy val expected0 = true
    lazy val pred0: Int => Boolean = x => ! (x == 7 )
    lazy val obtained0 = exampleSeq.forall (pred0 )
    assert (obtained0 == expected0 )

    lazy val expected1 = false
    lazy val pred1: Int => Boolean = x => x < 7
    lazy val obtained1 = exampleSeq.forall (pred1 )
    assert (obtained1 == expected1 )

    lazy val expected2 = true
    lazy val pred2: Int => Boolean = x => x == 7
    lazy val obtained2 = Seq () .forall (pred2 )
    assert (obtained2 == expected2 )
  }

  test ("forall") {
    lazy val expected0 = true
    lazy val pred0: Int => Boolean = x => ! (x == 7 )
    lazy val obtained0 = Min () .forall (example, pred0 )
    assert (obtained0 == expected0 )

    lazy val expected1 = false
    lazy val pred1: Int => Boolean = x => x < 7
    lazy val obtained1 = Min () .forall (example, pred1 )
    assert (obtained1 == expected1 )

    lazy val expected2 = true
    lazy val pred2: Int => Boolean = x => x == 7
    lazy val obtained2 = Min () .forall (empty, pred2 )
    assert (obtained2 == expected2 )
  }

  test ("exists with Seq") {
    lazy val expected0 = true
    lazy val pred0: Int => Boolean = x => x == 8
    lazy val obtained0 = exampleSeq.exists (pred0 )
    assert (obtained0 == expected0 )

    lazy val expected1 = false
    lazy val pred1: Int => Boolean = x => x == 7
    lazy val obtained1 = exampleSeq.exists (pred1 )
    assert (obtained1 == expected1 )

    lazy val expected2 = false
    lazy val pred2: Int => Boolean = x => x == 7
    lazy val obtained2 = Seq () .exists (pred2 )
    assert (obtained2 == expected2 )
  }

  test ("exists") {
    lazy val expected0 = true
    lazy val pred0: Int => Boolean = x => x == 8
    lazy val obtained0 = Min () .exists (example, pred0 )
    assert (obtained0 == expected0 )

    lazy val expected1 = false
    lazy val pred1: Int => Boolean = x => x == 7
    lazy val obtained1 = Min () .exists (example, pred1 )
    assert (obtained1 == expected1 )

    lazy val expected2 = false
    lazy val pred2: Int => Boolean = x => x == 7
    lazy val obtained2 = Min () .exists (empty, pred2 )
    assert (obtained2 == expected2 )
  }

  test ("find with Seq") {
    lazy val expected0 = Some (0 )
    lazy val pred0: Int => Boolean = x => ! (x == 7 )
    lazy val obtained0 = exampleSeq.find (pred0 )
    assert (obtained0 == expected0 )

    lazy val expected1 = Some (8 )
    lazy val pred1: Int => Boolean = x => x == 8
    lazy val obtained1 = exampleSeq.find (pred1 )
    assert (obtained1 == expected1 )

    lazy val expected2 = None
    lazy val pred2: Int => Boolean = x => x == 7
    lazy val obtained2 = Seq () .find (pred2 )
    assert (obtained2 == expected2 )
  }

  test ("find") {
    lazy val expected0 = Some (0 )
    lazy val pred0: Int => Boolean = x => ! (x == 7 )
    lazy val obtained0 = Min () .find (example, pred0 )
    assert (obtained0 == expected0 )

    lazy val expected1 = Some (8 )
    lazy val pred1: Int => Boolean = x => x == 8
    lazy val obtained1 = Min () .find (example, pred1 )
    assert (obtained1 == expected1 )

    lazy val expected2 = None
    lazy val pred2: Int => Boolean = x => x == 7
    lazy val obtained2 = Min () .find (empty, pred2 )
    assert (obtained2 == expected2 )
  }

  test ("filter with Seq") {
    lazy val expected = Seq (0, 3 )
    lazy val pred: Int => Boolean = x => x % 3 == 0
    lazy val obtained = exampleSeq.filter (pred )
    assert (obtained == expected )
  }

  test ("filter") {
    lazy val expected = MSeqTranslator () .asMSeq (Seq (0, 3 )  )
    lazy val f: Int => Boolean = x => x % 3 == 0
    lazy val obtained = Min () .filter (example, f )
    assert (obtained == expected )
  }

  test ("map with Seq") {
    lazy val expected = Seq (100, 101, 101, 102, 103, 105, 108 )
    lazy val f: Int => Int = x => x + 100
    lazy val obtained = exampleSeq.map (f )
    assert (obtained == expected )
  }

  test ("map in the same type") {
    lazy val expected = MSeqTranslator () .asMSeq (Seq (100, 101, 101, 102, 103, 105, 108 )  )
    lazy val f: Int => Int = x => x + 100
    lazy val obtained = Min () .map0 (example, f )
    assert (obtained == expected )
  }

  test ("foldLeft with Seq") {
    lazy val expected = Seq (108, 105, 103, 102, 101, 101, 100 )
    lazy val obtained = exampleSeq.foldLeft (Seq [Int]  ()  )  ((s: Seq [Int], e: Int ) => s.+: ((e + 100 )  )  )
    assert (obtained == expected )
  }

  test ("foldLeft in the same type") {
    lazy val expected = MSeqTranslator () .asMSeq (Seq (108, 105, 103, 102, 101, 101, 100 )  )
    lazy val obtained = Min () .foldLeft0 (example )  (Min () .empty, (s: MSeq [Int], e: Int ) => Min () .prepended (s, e + 100 )
    )
    assert (obtained == expected )
  }

}
