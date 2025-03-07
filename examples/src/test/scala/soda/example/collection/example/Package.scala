package soda.collection.example

/*
 * This package contains tests for examples for Soda.
 */

case class ListExampleSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("should test a") (
    check (
      obtained = ListExample_ () .a_example
    ) (
      expected = Pair .mk ("a") (List ('A' , 'B' , 'C' , 'D' , 'E' , 'F') )
    )
  )

  test ("should test b") (
    check (
      obtained = ListExample_ () .b_example
    ) (
      expected = Pair .mk ("b") (List (10 , 20 , 30 , 40 , 50 , 60) )
    )
  )

  test ("should test a .take") (
    check (
      obtained = ListExample_ () .take_example
    ) (
      expected = Pair .mk ("a .take (3)") (List ('A' , 'B' , 'C') )
    )
  )

  test ("should test a .takeRight") (
    check (
      obtained = ListExample_ () .takeRight_example
    ) (
      expected = Pair .mk ("a .takeRight (3)") (List ('D' , 'E' , 'F') )
    )
  )

  test ("should test a .takeWhile") (
    check (
      obtained = ListExample_ () .takeWhile_example
    ) (
      expected = Pair .mk ("a .takeWhile (lambda x --> not (x == 'E') )") (
        List ('A' , 'B' , 'C' , 'D') )
    )
  )

  test ("should test a .drop") (
    check (
      obtained = ListExample_ () .drop_example
    ) (
      expected = Pair .mk ("a .drop (2)") (List ('C' , 'D' , 'E' , 'F') )
    )
  )

  test ("should test a .dropRight") (
    check (
      obtained = ListExample_ () .dropRight_example
    ) (
     expected = Pair .mk ("a .dropRight (2)") (List ('A' , 'B' , 'C' , 'D') )
    )
  )

  test ("should test a .dropWhile") (
    check (
      obtained = ListExample_ () .dropWhile_example
    ) (
      expected = Pair .mk ("a .dropWhile (lambda x --> not (x == 'E'))") (List ('E' , 'F') )
    )
  )

  test ("should test a .splitAt") (
    check (
      obtained = ListExample_ () .splitAt_example
    ) (
      expected = Pair .mk ("a .splitAt (3)") ( (List ('A' , 'B' , 'C') ,
        List ('D' , 'E' , 'F') )
      )
    )
  )

  test ("should test a .indices") (
    check (
      obtained = ListExample_ () .indices_example
    ) (
      expected = Pair .mk ("a .indices") (Range (0 , 6) )
    )
  )

  test ("should test a .zipWithIndex") (
    check (
      obtained = ListExample_ () .zipWithIndex_example
    ) (
      expected = Pair .mk ("a .zipWithIndex") (List (('A' , 0) , ('B' , 1) , ('C' , 2) ,
       ('D' , 3) , ('E' , 4) , ('F', 5) ) )
    )
  )

  test ("should test a .zip") (
    check (
      obtained = ListExample_ () .zip_example
    ) (
      expected = Pair .mk ("a .zip (b)") (List (('A' , 10) , ('B' , 20) , ('C' , 30) ,
        ('D' , 40) , ('E' , 50) , ('F' , 60) ) )
    )
  )

  test ("should test a .reverse") (
    check (
      obtained = ListExample_ () .reverse_example
    ) (
      expected = Pair .mk ("a .reverse") (List ('F' , 'E' , 'D' , 'C' , 'B' , 'A') )
    )
  )

  test ("should test a .+") (
    check (
      obtained = ListExample_ () .prepended_example
    ) (
      expected = Pair .mk ("a .+: ('X')") (List ('X' , 'A' , 'B' , 'C' , 'D' , 'E' , 'F') )
    )
  )

  test ("should test a .:") (
    check (
      obtained = ListExample_ () .appended_example
    ) (
      expected = Pair .mk ("a .:+ ('X')") (List ('A' , 'B' , 'C' , 'D' , 'E' , 'F' , 'X') )
    )
  )

  test ("should test a .++") (
    check (
      obtained = ListExample_ () .concat_example
    ) (
      expected = Pair .mk ("a .map (lambda x --> x .toInt) .++ (b)") (
        List (65 , 66 , 67 , 68 , 69 , 70 , 10 , 20 , 30 , 40 , 50 , 60) )
    )
  )

  test ("should test a .span") (
    check (
      obtained = ListExample_ () .span_example
    ) (
      expected = Pair .mk ("a .span (lambda x --> not (x == 'D') )") (
        (List ('A' , 'B' , 'C') , List ('D' , 'E' , 'F') )
      )
    )
  )

  test ("should test a .map") (
    check (
      obtained = ListExample_ () .map_example
    ) (
      expected = Pair .mk ("a .map (lambda x --> x .toInt)") (
        List (65 , 66 , 67 , 68 , 69 , 70)
      )
    )
  )

  test ("should test a .filter") (
    check (
      obtained = ListExample_ () .filter_example
    ) (
      expected = Pair .mk ("a .filter (lambda x --> x .toInt % 2 == 0)") (
        List ('B' , 'D' , 'F')
      )
    )
  )

  test ("should test b .fold") (
    check (
      obtained = ListExample_ () .fold_example
    ) (
      expected = Pair .mk ("b .fold(0) (lambda (a , b) --> a + b)") (210)
    )
  )

  test ("should test a .foldLeft") (
    check (
      obtained = ListExample_ () .foldLeft_example
    ) (
      expected = Pair .mk ("a .foldLeft (Seq ('*') ) (lambda (list , elem) -->" +
        " \"(\" + list + \" :+ \" + elem + \")\")") (
        "((((((List(*) :+ A) :+ B) :+ C) :+ D) :+ E) :+ F)" .toCharArray .toSeq)
    )
  )

  test ("should test a .foldRight") (
    check (
      obtained = ListExample_ () .foldRight_example
    ) (
      expected = Pair .mk ("a .foldRight (Seq ('*') ) (lambda (elem , list) -->" +
        " \"(\" + elem + \" +: \" + list + \")\")") (
        "(A +: (B +: (C +: (D +: (E +: (F +: List(*)))))))" .toCharArray .toSeq)
    )
  )

}

