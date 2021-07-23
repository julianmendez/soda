package soda.collection


case class MinSpec () extends org.scalatest.funsuite.AnyFunSuite {
  import soda.lib.OptionSD
  import soda.lib.NoInst
  import soda.lib.SomeInst
  import soda.lib.Rec

  lazy val empty: ESeq [Int] = ESeqImpl [Int]  ()
  lazy val exampleSeq: Seq [Int] = Seq (0, 1, 1, 2, 3, 5, 8 )
  lazy val revExampleSeq: Seq [Int] = exampleSeq.reverse

  def prepend_elem (list: MSeq [Int], elem: Int ) = MinImpl () .prepended (list, elem )
  lazy val example: NESeq [Int] =
    Rec () .foldLeft (revExampleSeq.tail, NESeqImpl [Int]  (revExampleSeq.head, ESeqImpl [Int]  ()  ), prepend_elem )

  test ("prepended") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (1, 0, 1, 1, 2, 3, 5, 8 )  )
    lazy val obtained = MinImpl () .prepended (example, 1 )

    assert (obtained == expected )
  }

  test ("head") {
    lazy val expected = 0
    lazy val obtained = MinImpl () .head (example )

    assert (obtained == expected )
  }

  test ("tail") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (1, 1, 2, 3, 5, 8 )  )
    lazy val obtained = MinImpl () .tail (example )

    assert (obtained == expected )
  }

  //

  test ("reverse") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (8, 5, 3, 2, 1, 1, 0 )  )
    lazy val obtained = MinImpl () .reverse (example )

    assert (obtained == expected )
  }

  test ("length") {
    lazy val expected = 7
    lazy val obtained = MinImpl () .length (example )

    assert (obtained == expected )
  }

  test ("indexOf something found") {
    lazy val expected = 6
    lazy val obtained = MinImpl () .indexOf (example, 8 )

    assert (obtained == expected )
  }

  test ("indexOf something not found") {
    lazy val expected = -1
    lazy val obtained = MinImpl () .indexOf (example, 7 )

    assert (obtained == expected )
  }

  test ("contains something found") {
    lazy val expected = true
    lazy val obtained = MinImpl () .contains (example, 8 )

    assert (obtained == expected )
  }

  test ("contains something not found") {
    lazy val expected = false
    lazy val obtained = MinImpl () .contains (example, 7 )

    assert (obtained == expected )
  }

  test ("at") {
    lazy val expected = SomeInst (3 )
    lazy val obtained = MinImpl () .at (example, 4 )

    assert (obtained == expected )
  }

  //

  test ("take with Seq 0") {
    lazy val expected = Seq (0, 1, 1, 2, 3 )
    lazy val obtained = exampleSeq.take (5 )

    assert (obtained == expected )
  }

  test ("take with Seq 1") {
    lazy val expected = Seq ()
    lazy val obtained = exampleSeq.take (-100 )

    assert (obtained == expected )
  }

  test ("take with Seq 2") {
    lazy val expected = exampleSeq
    lazy val obtained = exampleSeq.take (100 )

    assert (obtained == expected )
  }

  test ("take 0") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (0, 1, 1, 2, 3 )  )
    lazy val obtained = MinImpl () .take (example, 5 )

    assert (obtained == expected )
  }

  test ("take 1") {
    lazy val expected = empty
    lazy val obtained = MinImpl () .take (example, -100 )

    assert (obtained == expected )
  }

  test ("take 2") {
    lazy val expected = example
    lazy val obtained = MinImpl () .take (example, 100 )

    assert (obtained == expected )
  }

  test ("drop with Seq 0") {
    lazy val expected = Seq (2, 3, 5, 8 )
    lazy val obtained = exampleSeq.drop (3 )

    assert (obtained == expected )
  }

  test ("drop with Seq 1") {
    lazy val expected = exampleSeq
    lazy val obtained = exampleSeq.drop (-100 )

    assert (obtained == expected )
  }

  test ("drop with Seq 2") {
    lazy val expected = Seq ()
    lazy val obtained = exampleSeq.drop (100 )

    assert (obtained == expected )
  }

  test ("drop 0") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (2, 3, 5, 8 )  )
    lazy val obtained = MinImpl () .drop (example, 3 )

    assert (obtained == expected )
  }

  test ("drop 1") {
    lazy val expected = example
    lazy val obtained = MinImpl () .drop (example, -100 )

    assert (obtained == expected )
  }

  test ("drop 2") {
    lazy val expected = empty
    lazy val obtained = MinImpl () .drop (example, 100 )

    assert (obtained == expected )
  }

  test ("takeWhile with Seq") {
    lazy val expected = Seq (0, 1, 1, 2 )
    lazy val f: Int => Boolean = e => ! (e == 3 )
    lazy val obtained = exampleSeq.takeWhile (f )

    assert (obtained == expected )
  }

  test ("takeWhile") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (0, 1, 1, 2 )  )
    lazy val f: Int => Boolean = e => ! (e == 3 )
    lazy val obtained = MinImpl () .takeWhile (example, f )

    assert (obtained == expected )
  }

  test ("dropWhile with Seq") {
    lazy val expected = Seq (3, 5, 8 )
    lazy val f: Int => Boolean = e => ! (e == 3 )
    lazy val obtained = exampleSeq.dropWhile (f )

    assert (obtained == expected )
  }

  test ("dropWhile") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (3, 5, 8 )  )
    lazy val f: Int => Boolean = e => ! (e == 3 )
    lazy val obtained = MinImpl () .dropWhile (example, f )

    assert (obtained == expected )
  }

  test ("splitAt with Seq") {
    lazy val expected = Tuple2 (Seq (0, 1, 1 ), Seq (2, 3, 5, 8 )  )
    lazy val obtained = exampleSeq.splitAt (3 )

    assert (obtained == expected )
  }

  test ("splitAt") {
    lazy val expected = MSeqPair (MSeqTranslatorImpl () .asMSeq (Seq (0, 1, 1 )  ), MSeqTranslatorImpl () .asMSeq (Seq (2, 3, 5, 8 )  )  )
    lazy val obtained = MinImpl () .splitAt (example, 3 )

    assert (obtained == expected )
  }

  test ("span with Seq") {
    lazy val expected = Tuple2 (Seq (0, 1, 1, 2, 3 ), Seq (5, 8 )  )
    lazy val obtained = exampleSeq.span (x => ! (x == 5 )  )

    assert (obtained == expected )
  }

  test ("span") {
    lazy val expected = MSeqPair (MSeqTranslatorImpl () .asMSeq (Seq (0, 1, 1, 2, 3 )  ), MSeqTranslatorImpl () .asMSeq (Seq (5, 8 )  )  )
    lazy val obtained = MinImpl () .span (example, (x: Int ) => ! (x == 5 )  )

    assert (obtained == expected )
  }

  //

  test ("append") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (0, 1, 1, 2, 3, 5, 8, 13 )  )
    lazy val obtained = MinImpl () .appended (example, 13 )

    assert (obtained == expected )
  }

  test ("last") {
    lazy val expected = 8
    lazy val obtained = MinImpl () .last (example )

    assert (obtained == expected )
  }

  test ("concat") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55 )  )
    lazy val second = MSeqTranslatorImpl () .asMSeq (Seq (13, 21, 34, 55 )  )
    lazy val obtained = MinImpl () .concat (example, second )

    assert (obtained == expected )
  }

  test ("slice with Seq 0") {
    lazy val expected = Seq (1, 2, 3 )
    lazy val obtained = exampleSeq.slice (2, 5 )

    assert (obtained == expected )
  }

  test ("slice with Seq 1") {
    lazy val expected = Seq ()
    lazy val obtained = exampleSeq.slice (5, 2 )

    assert (obtained == expected )
  }

  test ("slice with Seq 2") {
    lazy val expected = Seq ()
    lazy val obtained = exampleSeq.slice (3, 3 )

    assert (obtained == expected )
  }

  test ("slice with Seq 3") {
    lazy val expected = Seq ()
    lazy val obtained = exampleSeq.slice (100, 5 )

    assert (obtained == expected )
  }

  test ("slice with Seq 4") {
    lazy val expected = Seq ()
    lazy val obtained = exampleSeq.slice (100, 200 )

    assert (obtained == expected )
  }

  test ("slice with Seq 5") {
    lazy val expected = exampleSeq
    lazy val obtained = exampleSeq.slice (-100, 200 )

    assert (obtained == expected )
  }

  test ("slice 0") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (1, 2, 3 )  )
    lazy val obtained = MinImpl () .slice (example, 2, 5 )

    assert (obtained == expected )
  }

  test ("slice 1") {
    lazy val expected = empty
    lazy val obtained = MinImpl () .slice (example, 5, 2 )

    assert (obtained == expected )
  }

  test ("slice 2") {
    lazy val expected = empty
    lazy val obtained = MinImpl () .slice (example, 3, 3 )

    assert (obtained == expected )
  }

  test ("slice 3") {
    lazy val expected = empty
    lazy val obtained = MinImpl () .slice (example, 100, 5 )

    assert (obtained == expected )
  }

  test ("slice 4") {
    lazy val expected = empty
    lazy val obtained = MinImpl () .slice (example, 100, 200 )

    assert (obtained == expected )
  }

  test ("slice 5") {
    lazy val expected = example
    lazy val obtained = MinImpl () .slice (example, -100, 200 )

    assert (obtained == expected )
  }

  //

  test ("forall with Seq 0") {
    lazy val expected = true
    lazy val predicate: Int => Boolean = x => ! (x == 7 )
    lazy val obtained = exampleSeq.forall (predicate )

    assert (obtained == expected )
  }

  test ("forall with Seq 1") {
    lazy val expected = false
    lazy val predicate: Int => Boolean = x => x < 7
    lazy val obtained = exampleSeq.forall (predicate )

    assert (obtained == expected )
  }

  test ("forall with Seq 2") {
    lazy val expected = true
    lazy val predicate: Int => Boolean = x => x == 7
    lazy val obtained = Seq () .forall (predicate )

    assert (obtained == expected )
  }

  test ("forall 0") {
    lazy val expected = true
    lazy val predicate: Int => Boolean = x => ! (x == 7 )
    lazy val obtained = MinImpl () .forall (example, predicate )

    assert (obtained == expected )
  }

  test ("forall 1") {
    lazy val expected = false
    lazy val predicate: Int => Boolean = x => x < 7
    lazy val obtained = MinImpl () .forall (example, predicate )

    assert (obtained == expected )
  }

  test ("forall 2") {
    lazy val expected = true
    lazy val predicate: Int => Boolean = x => x == 7
    lazy val obtained = MinImpl () .forall (empty, predicate )

    assert (obtained == expected )
  }

  test ("exists with Seq 0") {
    lazy val expected = true
    lazy val predicate: Int => Boolean = x => x == 8
    lazy val obtained = exampleSeq.exists (predicate )

    assert (obtained == expected )
  }

  test ("exists with Seq 1") {
    lazy val expected = false
    lazy val predicate: Int => Boolean = x => x == 7
    lazy val obtained = exampleSeq.exists (predicate )

    assert (obtained == expected )
  }

  test ("exists with Seq 2") {
    lazy val expected = false
    lazy val predicate: Int => Boolean = x => x == 7
    lazy val obtained = Seq () .exists (predicate )

    assert (obtained == expected )
  }

  test ("exists 0") {
    lazy val expected = true
    lazy val predicate: Int => Boolean = x => x == 8
    lazy val obtained = MinImpl () .exists (example, predicate )

    assert (obtained == expected )
  }

  test ("exists 1") {
    lazy val expected = false
    lazy val predicate: Int => Boolean = x => x == 7
    lazy val obtained = MinImpl () .exists (example, predicate )

    assert (obtained == expected )
  }

  test ("exists 2") {
    lazy val expected = false
    lazy val predicate: Int => Boolean = x => x == 7
    lazy val obtained = MinImpl () .exists (empty, predicate )

    assert (obtained == expected )
  }

  test ("find with Seq 0") {
    lazy val expected = Some (0 )
    lazy val predicate: Int => Boolean = x => ! (x == 7 )
    lazy val obtained = exampleSeq.find (predicate )

    assert (obtained == expected )
  }

  test ("find with Seq 1") {
    lazy val expected = Some (8 )
    lazy val predicate: Int => Boolean = x => x == 8
    lazy val obtained = exampleSeq.find (predicate )

    assert (obtained == expected )
  }

  test ("find with Seq 2") {
    lazy val expected = None
    lazy val predicate: Int => Boolean = x => x == 7
    lazy val obtained = Seq () .find (predicate )

    assert (obtained == expected )
  }

  test ("find 0") {
    lazy val expected = SomeInst (0 )
    lazy val predicate: Int => Boolean = x => ! (x == 7 )
    lazy val obtained = MinImpl () .find (example, predicate )

    assert (obtained == expected )
  }

  test ("find 1") {
    lazy val expected = SomeInst (8 )
    lazy val predicate: Int => Boolean = x => x == 8
    lazy val obtained = MinImpl () .find (example, predicate )

    assert (obtained == expected )
  }

  test ("find 2") {
    lazy val expected = NoInst ()
    lazy val predicate: Int => Boolean = x => x == 7
    lazy val obtained = MinImpl () .find (empty, predicate )

    assert (obtained == expected )
  }

  test ("filter with Seq") {
    lazy val expected = Seq (0, 3 )
    lazy val predicate: Int => Boolean = x => x % 3 == 0
    lazy val obtained = exampleSeq.filter (predicate )

    assert (obtained == expected )
  }

  test ("filter") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (0, 3 )  )
    lazy val f: Int => Boolean = x => x % 3 == 0
    lazy val obtained = MinImpl () .filter (example, f )

    assert (obtained == expected )
  }

  test ("map with Seq") {
    lazy val expected = Seq (100, 101, 101, 102, 103, 105, 108 )
    lazy val f: Int => Int = x => x + 100
    lazy val obtained = exampleSeq.map (f )

    assert (obtained == expected )
  }

  test ("map in the same type") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (100, 101, 101, 102, 103, 105, 108 )  )
    lazy val f: Int => Int = x => x + 100
    lazy val obtained = MinImpl () .map0 (example, f )

    assert (obtained == expected )
  }

  test ("foldLeft with Seq") {
    lazy val expected = Seq (108, 105, 103, 102, 101, 101, 100 )
    lazy val obtained = exampleSeq.foldLeft (Seq [Int]  ()  )  ((s: Seq [Int], e: Int ) => s.+: ((e + 100 )  )  )

    assert (obtained == expected )
  }

  test ("foldLeft in the same type") {
    lazy val expected = MSeqTranslatorImpl () .asMSeq (Seq (108, 105, 103, 102, 101, 101, 100 )  )
    lazy val obtained = MinImpl () .foldLeft0 (example )  (MinImpl () .empty, (s: MSeq [Int], e: Int ) => MinImpl () .prepended (s, e + 100 )    )

    assert (obtained == expected )
  }
}
