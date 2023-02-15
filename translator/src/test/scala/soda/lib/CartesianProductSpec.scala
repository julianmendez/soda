package soda.lib

/*
 * This package contains tests for the Soda library.
 */



case class CartesianProductSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val int_seq_a = Seq (10, 20)

  lazy val int_seq_b = Seq (100, 200, 300)

  lazy val str_seq_a = Seq ("A", "B")

  lazy val str_seq_b = Seq ("0", "1", "2")

  lazy val str_seq_c = Seq ("a", "b", "c", "d")

  lazy val instance = CartesianProduct_ ()

  test ("Cartesian product of two sequences") (
    check (
      obtained = instance.apply (Seq (int_seq_a, int_seq_b) )
    ) (
      expected = Seq (
        Seq (10, 100), Seq (10, 200), Seq (10, 300),
        Seq (20, 100), Seq (20, 200), Seq (20, 300)
      )
    )
  )

  test ("Cartesian product of an empty sequence") (
    check (
      obtained = instance.apply (Seq () )
    ) (
      expected = Seq ()
    )
  )

  test ("Cartesian product of only empty sequences") (
    check (
      obtained = instance.apply (Seq (Seq (), Seq (), Seq () ) )
    ) (
      expected = Seq ()
    )
  )

  test ("Cartesian product with at least one empty sequence") (
    check (
      obtained = instance.apply (Seq (Seq ("A"), Seq () ) )
    ) (
      expected = Seq ()
    )
  )

  test ("Cartesian product of three sequences") (
    check (
      obtained = instance.apply (Seq (str_seq_a, str_seq_b, str_seq_c) )
    ) (
      expected = Seq (
        Seq ("A", "0", "a"), Seq ("A", "0", "b"), Seq ("A", "0", "c"), Seq ("A", "0", "d"),
        Seq ("A", "1", "a"), Seq ("A", "1", "b"), Seq ("A", "1", "c"), Seq ("A", "1", "d"),
        Seq ("A", "2", "a"), Seq ("A", "2", "b"), Seq ("A", "2", "c"), Seq ("A", "2", "d"),
        Seq ("B", "0", "a"), Seq ("B", "0", "b"), Seq ("B", "0", "c"), Seq ("B", "0", "d"),
        Seq ("B", "1", "a"), Seq ("B", "1", "b"), Seq ("B", "1", "c"), Seq ("B", "1", "d"),
        Seq ("B", "2", "a"), Seq ("B", "2", "b"), Seq ("B", "2", "c"), Seq ("B", "2", "d")
      )
    )
  )

}
