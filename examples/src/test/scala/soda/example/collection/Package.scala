package soda.example.collection

/*
 * This package contains tests for examples for Soda.
 */

case class MinSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.lib.NoneSD
  import   soda.lib.Fold
  import   soda.lib.SomeSD

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val empty : ESeq [Int] = ESeq_ [Int] ()

  lazy val exampleSeq : Seq [Int] = Seq (0 , 1 , 1 , 2 , 3 , 5 , 8)

  lazy val revExampleSeq : Seq [Int] = exampleSeq.reverse

  def prepend_elem (list : MSeq [Int] ) (elem : Int) : NESeq [Int] =
    Min .mk .prepended (list) (elem)

  private lazy val _fold = Fold .mk

  private lazy val _initial_value : NESeq [Int] = NESeq_ [Int] (revExampleSeq .head , ESeq_ [Int] () )

  lazy val example : NESeq [Int] =
    _fold .apply (revExampleSeq .tail) (_initial_value) (prepend_elem)

  test ("prepended") (
    check (
      obtained = _prepended_sequence
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq [Int] (1 , 0 , 1 , 1 , 2 , 3 , 5 , 8) )
    )
  )

  private lazy val _prepended_sequence : MSeq [Int] = Min .mk .prepended (example) (1)

  test ("head") (
    check (
      obtained = Min .mk .head (example)
    ) (
      expected = 0
    )
  )

  test ("tail") (
    check (
      obtained = Min .mk .tail (example)
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (1 , 1 , 2 , 3 , 5 , 8) )
    )
  )

  test ("reverse") (
    check (
      obtained = Min .mk .reverse (example)
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (8 , 5 , 3 , 2 , 1 , 1 , 0) )
    )
  )

  test ("length") (
    check (
      obtained = Min .mk .length (example)
    ) (
      expected = 7
    )
  )

  test ("indexOf something found") (
    check (
      obtained = Min .mk .indexOf (example) (8)
    ) (
      expected = 6
    )
  )

  test ("indexOf something not found") (
    check (
      obtained = Min .mk .indexOf (example) (7)
    ) (
      expected = -1
    )
  )

  test ("contains something found") (
    check (
      obtained = Min .mk .contains (example) (8)
    ) (
      expected = true
    )
  )

  test ("contains something not found") (
    check (
      obtained = Min .mk .contains (example) (7)
    ) (
      expected = false
    )
  )

  test ("at") (
    check (
      obtained = Min .mk .at (example) (4)
    ) (
      expected = SomeSD .mk [Int] (3)
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
      obtained = Min .mk .take (example) (5)
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (0 , 1 , 1 , 2 , 3) )
    )
  )

  test ("take 1") (
    check (
      obtained = Min .mk .take (example) (-100)
    ) (
      expected = empty
    )
  )

  test ("take 2") (
    check (
      obtained = Min .mk .take (example) (100)
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
      obtained = Min .mk .drop (example) (3)
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (2 , 3 , 5 , 8) )
    )
  )

  test ("drop 1") (
    check (
      obtained = Min .mk .drop (example) (-100)
    ) (
      expected = example
    )
  )

  test ("drop 2") (
    check (
      obtained = Min .mk .drop (example) (100)
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
      obtained = Min .mk .takeWhile (example) ( e => ! (e == 3) )
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (0 , 1 , 1 , 2) )
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
      obtained = Min .mk .dropWhile (example) ( e => ! (e == 3) )
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (3 , 5 , 8) )
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
      obtained = Min .mk .splitAt (example) (3)
    ) (
      expected = MSeqPair .mk (MSeqTranslator .mk .asMSeq (Seq (0 , 1 , 1) ) ) (
        MSeqTranslator .mk .asMSeq (Seq (2 , 3 , 5 , 8) ) )
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
      obtained = Min .mk .span (example) ( (x : Int) => ! (x == 5) )
    ) (
      expected = MSeqPair .mk (MSeqTranslator .mk .asMSeq (Seq (0 , 1 , 1 , 2 , 3) ) ) (
        MSeqTranslator .mk .asMSeq (Seq (5 , 8) ) )
    )
  )

  test ("append") (
    check (
      obtained = Min .mk .appended (example) (13)
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (0 , 1 , 1 , 2 , 3 , 5 , 8 , 13) )
    )
  )

  test ("last") (
    check (
      obtained = Min .mk .last (example)
    ) (
      expected = 8
    )
  )

  test ("concat") (
    check (
      obtained = Min .mk .concat (example) (MSeqTranslator .mk
        .asMSeq (Seq (13 , 21 , 34 , 55) ) )
    ) (
      expected = MSeqTranslator .mk
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
      obtained = Min .mk .slice (example) (2) (5)
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (1 , 2 , 3) )
    )
  )

  test ("slice 1") (
    check (
      obtained = Min .mk .slice (example) (5) (2)
    ) (
      expected = empty
    )
  )

  test ("slice 2") (
    check (
      obtained = Min .mk .slice (example) (3) (3)
    ) (
      expected = empty
    )
  )

  test ("slice 3") (
    check (
      obtained = Min .mk .slice (example) (100) (5)
    ) (
      expected = empty
    )
  )

  test ("slice 4") (
    check (
      obtained = Min .mk .slice (example) (100) (200)
    ) (
      expected = empty
    )
  )

  test ("slice 5") (
    check (
      obtained = Min .mk .slice (example) (-100) (200)
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
      obtained = Min .mk .forall (example) ( x => ! (x == 7) )
    ) (
      expected = true
    )
  )

  test ("forall 1") (
    check (
      obtained = Min .mk .forall (example) ( x => x < 7)
    ) (
      expected = false
    )
  )

  test ("forall 2") (
    check (
      obtained = Min .mk .forall (empty) ( x => x == 7)
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
      obtained = Min .mk .exists (example) ( x => x == 8)
    ) (
      expected = true
    )
  )

  test ("exists 1") (
    check (
      obtained = Min .mk .exists (example) ( x => x == 7)
    ) (
      expected = false
    )
  )

  test ("exists 2") (
    check (
      obtained = Min .mk .exists (empty) ( x => x == 7)
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
      obtained = Seq [Int] () .find ( x => x == 7)
    ) (
      expected = None
    )
  )

  test ("find 0") (
    check (
      obtained = Min .mk .find (example) ( x => ! (x == 7) )
    ) (
      expected = SomeSD .mk [Int] (0)
    )
  )

  test ("find 1") (
    check (
      obtained = Min .mk .find (example) ( x => x == 8)
    ) (
      expected = SomeSD .mk [Int] (8)
    )
  )

  test ("find 2") (
    check (
      obtained = Min .mk .find (empty) ( x => x == 7)
    ) (
      expected = NoneSD .mk
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
      obtained = Min .mk .filter (example) ( x => x % 3 == 0)
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (0 , 3) )
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
      obtained = Min .mk .map0 (example) ( x => x + 100)
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (100 , 101 , 101 , 102 , 103 , 105 , 108) )
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
      obtained = Min .mk .foldLeft0 (example) (Min .mk .empty) (
          (s : MSeq [Int] ) =>  (e : Int) => Min .mk .prepended (s) (e + 100)
      )
    ) (
      expected = MSeqTranslator .mk .asMSeq (Seq (108 , 105 , 103 , 102 , 101 , 101 , 100) )
    )
  )

}

