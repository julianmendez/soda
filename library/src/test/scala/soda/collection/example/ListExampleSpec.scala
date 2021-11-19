package soda.collection.example


case class ListExampleSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("should test a") {
    lazy val expected = Pair_ ("a", List ('A', 'B', 'C', 'D', 'E', 'F')  )
    lazy val obtained = ListExample_ () .a_example

    assert (obtained == expected )

}

  test ("should test b") {
    lazy val expected = Pair_ ("b", List (10, 20, 30, 40, 50, 60 )  )
    lazy val obtained = ListExample_ () .b_example

    assert (obtained == expected )

}

  test ("should test a.take") {
    lazy val expected = Pair_ ("a.take(3)", List ('A', 'B', 'C')  )
    lazy val obtained = ListExample_ () .take_example

    assert (obtained == expected )

}

  test ("should test a.takeRight") {
    lazy val expected = Pair_ ("a.takeRight(3)", List ('D', 'E', 'F')  )
    lazy val obtained = ListExample_ () .takeRight_example

    assert (obtained == expected )

}

  test ("should test a.takeWhile") {
    lazy val expected = Pair_ ("a.takeWhile(x -> not (x == 'E'))", List ('A', 'B', 'C', 'D')  )
    lazy val obtained = ListExample_ () .takeWhile_example

    assert (obtained == expected )

}

  test ("should test a.drop") {
    lazy val expected = Pair_ ("a.drop(2)", List ('C', 'D', 'E', 'F')  )
    lazy val obtained = ListExample_ () .drop_example

    assert (obtained == expected )

}

  test ("should test a.dropRight") {
    lazy val expected = Pair_ ("a.dropRight(2)", List ('A', 'B', 'C', 'D')  )
    lazy val obtained = ListExample_ () .dropRight_example

    assert (obtained == expected )

}

  test ("should test a.dropWhile") {
    lazy val expected = Pair_ ("a.dropWhile(x -> not (x == 'E'))", List ('E', 'F')  )
    lazy val obtained = ListExample_ () .dropWhile_example

    assert (obtained == expected )

}

  test ("should test a.splitAt") {
    lazy val expected = Pair_ ("a.splitAt(3)", (List ('A', 'B', 'C'), List ('D', 'E', 'F')  )  )
    lazy val obtained = ListExample_ () .splitAt_example

    assert (obtained == expected )

}

  test ("should test a.indices") {
    lazy val expected = Pair_ ("a.indices", Range (0, 6 )  )
    lazy val obtained = ListExample_ () .indices_example

    assert (obtained == expected )

}

  test ("should test a.zipWithIndex") {
    lazy val expected = Pair_ ("a.zipWithIndex", List (('A', 0 ), ('B', 1 ), ('C', 2 ), ('D', 3 ), ('E', 4 ), ('F', 5 )  )  )
    lazy val obtained = ListExample_ () .zipWithIndex_example

    assert (obtained == expected )

}

  test ("should test a.zip") {
    lazy val expected = Pair_ ("a.zip(b)", List (('A', 10 ), ('B', 20 ), ('C', 30 ), ('D', 40 ), ('E', 50 ), ('F', 60 )  )  )
    lazy val obtained = ListExample_ () .zip_example

    assert (obtained == expected )

}

  test ("should test a.reverse") {
    lazy val expected = Pair_ ("a.reverse", List ('F', 'E', 'D', 'C', 'B', 'A')  )
    lazy val obtained = ListExample_ () .reverse_example

    assert (obtained == expected )

}

  test ("should test a.+") {
    lazy val expected = Pair_ ("a.+:('X')", List ('X', 'A', 'B', 'C', 'D', 'E', 'F')  )
    lazy val obtained = ListExample_ () .prepended_example

    assert (obtained == expected )

}

  test ("should test a.:") {
    lazy val expected = Pair_ ("a.:+('X')", List ('A', 'B', 'C', 'D', 'E', 'F', 'X')  )
    lazy val obtained = ListExample_ () .appended_example

    assert (obtained == expected )

}

  test ("should test a.++") {
    lazy val expected = Pair_ ("a.map(_.toInt).++(b)", List (65, 66, 67, 68, 69, 70, 10, 20, 30, 40, 50, 60 )  )
    lazy val obtained = ListExample_ () .concat_example

    assert (obtained == expected )

}

  test ("should test a.span") {
    lazy val expected = Pair_ ("a.span(x -> not (x == 'D'))", (List ('A', 'B', 'C'), List ('D', 'E', 'F')  )  )
    lazy val obtained = ListExample_ () .span_example

    assert (obtained == expected )

}

  test ("should test a.map") {
    lazy val expected = Pair_ ("a.map(x -> x.toInt)", List (65, 66, 67, 68, 69, 70 )  )
    lazy val obtained = ListExample_ () .map_example

    assert (obtained == expected )

}

  test ("should test a.filter") {
    lazy val expected = Pair_ ("a.filter(x -> x.toInt % 2 == 0)", List ('B', 'D', 'F')  )
    lazy val obtained = ListExample_ () .filter_example

    assert (obtained == expected )

}

  test ("should test b.fold") {
    lazy val expected = Pair_ ("b.fold(0)((a, b) -> a + b)", 210 )
    lazy val obtained = ListExample_ () .fold_example

    assert (obtained == expected )

}

  test ("should test a.foldLeft") {
    lazy val expected = Pair_ ("a.foldLeft(Seq('*'))((list, elem) -> \"(\" + list + \" :+ \" + elem + \")\")", "((((((List(*) :+ A) :+ B) :+ C) :+ D) :+ E) :+ F)".toCharArray.toSeq )
    lazy val obtained = ListExample_ () .foldLeft_example

    assert (obtained == expected )

}

  test ("should test a.foldRight") {
    lazy val expected = Pair_ ("a.foldRight(Seq('*'))((elem, list) -> \"(\" + elem + \" +: \" + list + \")\")", "(A +: (B +: (C +: (D +: (E +: (F +: List(*)))))))".toCharArray.toSeq )
    lazy val obtained = ListExample_ () .foldRight_example

    assert (obtained == expected )

}

}
