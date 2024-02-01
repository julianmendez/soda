package soda.collection

/*
 * This package contains tests for examples for Soda.
 */

case class MinSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.lib.NoneSD_
  import   soda.lib.Fold_
  import   soda.lib.SomeSD_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val empty : ESeq [Int] = ESeq_ [Int] ()

  lazy val exampleSeq : Seq [Int] = Seq (0 , 1 , 1 , 2 , 3 , 5 , 8)

  lazy val revExampleSeq : Seq [Int] = exampleSeq.reverse

  def prepend_elem (list : MSeq [Int] ) (elem : Int) : NESeq [Int] =
    Min_ () .prepended (list) (elem)

  private lazy val _fold = Fold_ ()

  private lazy val _initial_value : NESeq [Int] = NESeq_ [Int] (revExampleSeq .head , ESeq_ [Int] () )

  lazy val example : NESeq [Int] =
    _fold .apply (revExampleSeq .tail) (_initial_value) (prepend_elem)

  test ("prepended") (
    check (
      obtained = _prepended_sequence
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq [Int] (1 , 0 , 1 , 1 , 2 , 3 , 5 , 8) )
    )
  )

  private lazy val _prepended_sequence : MSeq [Int] = Min_ () .prepended (example) (1)

  test ("head") (
    check (
      obtained = Min_ () .head (example)
    ) (
      expected = 0
    )
  )

  test ("tail") (
    check (
      obtained = Min_ () .tail (example)
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (1 , 1 , 2 , 3 , 5 , 8) )
    )
  )

  test ("reverse") (
    check (
      obtained = Min_ () .reverse (example)
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (8 , 5 , 3 , 2 , 1 , 1 , 0) )
    )
  )

  test ("length") (
    check (
      obtained = Min_ () .length (example)
    ) (
      expected = 7
    )
  )

  test ("indexOf something found") (
    check (
      obtained = Min_ () .indexOf (example) (8)
    ) (
      expected = 6
    )
  )

  test ("indexOf something not found") (
    check (
      obtained = Min_ () .indexOf (example) (7)
    ) (
      expected = -1
    )
  )

  test ("contains something found") (
    check (
      obtained = Min_ () .contains (example) (8)
    ) (
      expected = true
    )
  )

  test ("contains something not found") (
    check (
      obtained = Min_ () .contains (example) (7)
    ) (
      expected = false
    )
  )

  test ("at") (
    check (
      obtained = Min_ () .at (example) (4)
    ) (
      expected = SomeSD_ (3)
    )
  )

  test ("take with Seq 0") (
    check (
      obtained = exampleSeq .take (5)
    ) (
      expected = Seq (0 , 1 , 1 , 2 , 3)
    )
  )

  test ("take with Seq 1") (
    check (
      obtained = exampleSeq .take (-100)
    ) (
      expected = Seq ()
    )
  )

  test ("take with Seq 2") (
    check (
      obtained = exampleSeq .take (100)
    ) (
      expected = exampleSeq
    )
  )

  test ("take 0") (
    check (
      obtained = Min_ () .take (example) (5)
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (0 , 1 , 1 , 2 , 3) )
    )
  )

  test ("take 1") (
    check (
      obtained = Min_ () .take (example) (-100)
    ) (
      expected = empty
    )
  )

  test ("take 2") (
    check (
      obtained = Min_ () .take (example) (100)
    ) (
      expected = example
    )
  )

  test ("drop with Seq 0") (
    check (
      obtained = exampleSeq .drop (3)
    ) (
      expected = Seq (2 , 3 , 5 , 8)
    )
  )

  test ("drop with Seq 1") (
    check (
      obtained = exampleSeq .drop (-100)
    ) (
      expected = exampleSeq
    )
  )

  test ("drop with Seq 2") (
    check (
      obtained = exampleSeq .drop (100)
    ) (
      expected = Seq ()
    )
  )

  test ("drop 0") (
    check (
      obtained = Min_ () .drop (example) (3)
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (2 , 3 , 5 , 8) )
    )
  )

  test ("drop 1") (
    check (
      obtained = Min_ () .drop (example) (-100)
    ) (
      expected = example
    )
  )

  test ("drop 2") (
    check (
      obtained = Min_ () .drop (example) (100)
    ) (
      expected = empty
    )
  )

  test ("takeWhile with Seq") (
    check (
      obtained = exampleSeq .takeWhile ( e => ! (e == 3) )
    ) (
      expected = Seq (0 , 1 , 1 , 2)
    )
  )

  test ("takeWhile") (
    check (
      obtained = Min_ () .takeWhile (example) ( e => ! (e == 3) )
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (0 , 1 , 1 , 2) )
    )
  )

  test ("dropWhile with Seq") (
    check (
      obtained = exampleSeq .dropWhile ( e => ! (e == 3) )
    ) (
      expected = Seq (3 , 5 , 8)
    )
  )

  test ("dropWhile") (
    check (
      obtained = Min_ () .dropWhile (example) ( e => ! (e == 3) )
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (3 , 5 , 8) )
    )
  )

  test ("splitAt with Seq") (
    check (
      obtained = exampleSeq .splitAt (3)
    ) (
      expected = Tuple2 (Seq (0 , 1 , 1) , Seq (2 , 3 , 5 , 8) )
    )
  )

  test ("splitAt") (
    check (
      obtained = Min_ () .splitAt (example) (3)
    ) (
      expected = MSeqPair_ (MSeqTranslator_ () .asMSeq (Seq (0 , 1 , 1) ) ,
        MSeqTranslator_ () .asMSeq (Seq (2 , 3 , 5 , 8) ) )
    )
  )

  test ("span with Seq") (
    check (
      obtained = exampleSeq .span ( x => ! (x == 5) )
    ) (
      expected = Tuple2 (Seq (0 , 1 , 1 , 2 , 3) , Seq (5 , 8) )
    )
  )

  test ("span") (
    check (
      obtained = Min_ () .span (example) ( (x : Int) => ! (x == 5) )
    ) (
      expected = MSeqPair_ (MSeqTranslator_ () .asMSeq (Seq (0 , 1 , 1 , 2 , 3) ) ,
        MSeqTranslator_ () .asMSeq (Seq (5 , 8) ) )
    )
  )

  test ("append") (
    check (
      obtained = Min_ () .appended (example) (13)
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (0 , 1 , 1 , 2 , 3 , 5 , 8 , 13) )
    )
  )

  test ("last") (
    check (
      obtained = Min_ () .last (example)
    ) (
      expected = 8
    )
  )

  test ("concat") (
    check (
      obtained = Min_ () .concat (example) (MSeqTranslator_ ()
        .asMSeq (Seq (13 , 21 , 34 , 55) ) )
    ) (
      expected = MSeqTranslator_ ()
        .asMSeq (Seq (0 , 1 , 1 , 2 , 3 , 5 , 8 , 13 , 21 , 34 , 55) )
    )
  )

  test ("slice with Seq 0") (
    check (
      obtained = exampleSeq .slice (2 , 5)
    ) (
      expected = Seq (1 , 2 , 3)
    )
  )

  test ("slice with Seq 1") (
    check (
      obtained = exampleSeq .slice (5 , 2)
    ) (
      expected = Seq ()
    )
  )

  test ("slice with Seq 2") (
    check (
      obtained = exampleSeq .slice (3 , 3)
    ) (
      expected = Seq ()
    )
  )

  test ("slice with Seq 3") (
    check (
      obtained = exampleSeq .slice (100 , 5)
    ) (
      expected = Seq ()
    )
  )

  test ("slice with Seq 4") (
    check (
      obtained = exampleSeq .slice (100 , 200)
    ) (
      expected = Seq ()
    )
  )

  test ("slice with Seq 5") (
    check (
      obtained = exampleSeq .slice (-100 , 200)
    ) (
      expected = exampleSeq
    )
  )

  test ("slice 0") (
    check (
      obtained = Min_ () .slice (example) (2) (5)
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (1 , 2 , 3) )
    )
  )

  test ("slice 1") (
    check (
      obtained = Min_ () .slice (example) (5) (2)
    ) (
      expected = empty
    )
  )

  test ("slice 2") (
    check (
      obtained = Min_ () .slice (example) (3) (3)
    ) (
      expected = empty
    )
  )

  test ("slice 3") (
    check (
      obtained = Min_ () .slice (example) (100) (5)
    ) (
      expected = empty
    )
  )

  test ("slice 4") (
    check (
      obtained = Min_ () .slice (example) (100) (200)
    ) (
      expected = empty
    )
  )

  test ("slice 5") (
    check (
      obtained = Min_ () .slice (example) (-100) (200)
    ) (
      expected = example
    )
  )

  test ("forall with Seq 0") (
    check (
      obtained = exampleSeq .forall ( x => ! (x == 7) )
    ) (
      expected = true
    )
  )

  test ("forall with Seq 1") (
    check (
      obtained = exampleSeq .forall ( x => x < 7)
    ) (
      expected = false
    )
  )

  test ("forall with Seq 2") (
    check (
      obtained = Seq [Int] () .forall ( x => x == 7)
    ) (
      expected = true
    )
  )

  test ("forall 0") (
    check (
      obtained = Min_ () .forall (example) ( x => ! (x == 7) )
    ) (
      expected = true
    )
  )

  test ("forall 1") (
    check (
      obtained = Min_ () .forall (example) ( x => x < 7)
    ) (
      expected = false
    )
  )

  test ("forall 2") (
    check (
      obtained = Min_ () .forall (empty) ( x => x == 7)
    ) (
      expected = true
    )
  )

  test ("exists with Seq 0") (
    check (
      obtained = exampleSeq .exists ( x => x == 8)
    ) (
      expected = true
    )
  )

  test ("exists with Seq 1") (
    check (
      obtained = exampleSeq .exists ( x => x == 7)
    ) (
      expected = false
    )
  )

  test ("exists with Seq 2") (
    check (
      obtained = Seq [Int] () .exists ( x => x == 7)
    ) (
      expected = false
    )
  )

  test ("exists 0") (
    check (
      obtained = Min_ () .exists (example) ( x => x == 8)
    ) (
      expected = true
    )
  )

  test ("exists 1") (
    check (
      obtained = Min_ () .exists (example) ( x => x == 7)
    ) (
      expected = false
    )
  )

  test ("exists 2") (
    check (
      obtained = Min_ () .exists (empty) ( x => x == 7)
    ) (
      expected = false
    )
  )

  test ("find with Seq 0") (
    check (
      obtained = exampleSeq .find ( x => ! (x == 7) )
    ) (
      expected = Some (0)
    )
  )

  test ("find with Seq 1") (
    check (
      obtained = exampleSeq .find ( x => x == 8)
    ) (
      expected = Some (8)
    )
  )

  test ("find with Seq 2") (
    check (
      obtained = Seq () .find ( x => x == 7)
    ) (
      expected = None
    )
  )

  test ("find 0") (
    check (
      obtained = Min_ () .find (example) ( x => ! (x == 7) )
    ) (
      expected = SomeSD_ (0)
    )
  )

  test ("find 1") (
    check (
      obtained = Min_ () .find (example) ( x => x == 8)
    ) (
      expected = SomeSD_ (8)
    )
  )

  test ("find 2") (
    check (
      obtained = Min_ () .find (empty) ( x => x == 7)
    ) (
      expected = NoneSD_ ()
    )
  )

  test ("filter with Seq") (
    check (
      obtained = exampleSeq .filter ( x => x % 3 == 0)
    ) (
      expected = Seq (0 , 3)
    )
  )

  test ("filter") (
    check (
      obtained = Min_ () .filter (example) ( x => x % 3 == 0)
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (0 , 3) )
    )
  )

  test ("map with Seq") (
    check (
      obtained = exampleSeq .map ( x => x + 100)
    ) (
      expected = Seq (100 , 101 , 101 , 102 , 103 , 105 , 108)
    )
  )

  test ("map in the same type") (
    check (
      obtained = Min_ () .map0 (example) ( x => x + 100)
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (100 , 101 , 101 , 102 , 103 , 105 , 108) )
    )
  )

  test ("foldLeft with Seq") (
    check (
      obtained = exampleSeq .foldLeft (Seq [Int] () ) ( (s : Seq [Int] , e : Int) =>
        s .+: ( (e + 100) ) )
    ) (
      expected = Seq (108 , 105 , 103 , 102 , 101 , 101 , 100)
    )
  )

  test ("foldLeft in the same type") (
    check (
      obtained = Min_ () .foldLeft0 (example) (Min_ () .empty) (
          (s : MSeq [Int] ) =>  (e : Int) => Min_ () .prepended (s) (e + 100)
      )
    ) (
      expected = MSeqTranslator_ () .asMSeq (Seq (108 , 105 , 103 , 102 , 101 , 101 , 100) )
    )
  )

}

