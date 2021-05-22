package soda.collection.example


case class ListExampleSpec () extends org.scalatest.funsuite.AnyFunSuite {

  test ("should test A") {
    lazy val expected = Pair ("A", List ('A', 'B', 'C', 'D', 'E', 'F')  )
    lazy val obtained = ListExample () .aExample
    assert (obtained == expected )
  }

  test ("should test B") {
    lazy val expected = Pair ("B", List (10, 20, 30, 40, 50, 60 )  )
    lazy val obtained = ListExample () .bExample
    assert (obtained == expected )
  }

  test ("should test A.take") {
    lazy val expected = Pair ("A.take(3)", List ('A', 'B', 'C')  )
    lazy val obtained = ListExample () .takeExample
    assert (obtained == expected )
  }

  test ("should test A.takeRight") {
    lazy val expected = Pair ("A.takeRight(3)", List ('D', 'E', 'F')  )
    lazy val obtained = ListExample () .takeRightExample
    assert (obtained == expected )
  }

  test ("should test A.takeWhile") {
    lazy val expected = Pair ("A.takeWhile(x -> not (x == 'E'))", List ('A', 'B', 'C', 'D')  )
    lazy val obtained = ListExample () .takeWhileExample
    assert (obtained == expected )
  }

  test ("should test A.drop") {
    lazy val expected = Pair ("A.drop(2)", List ('C', 'D', 'E', 'F')  )
    lazy val obtained = ListExample () .dropExample
    assert (obtained == expected )
  }

  test ("should test A.dropRight") {
    lazy val expected = Pair ("A.dropRight(2)", List ('A', 'B', 'C', 'D')  )
    lazy val obtained = ListExample () .dropRightExample
    assert (obtained == expected )
  }

  test ("should test A.dropWhile") {
    lazy val expected = Pair ("A.dropWhile(x -> not (x == 'E'))", List ('E', 'F')  )
    lazy val obtained = ListExample () .dropWhileExample
    assert (obtained == expected )
  }

  test ("should test A.splitAt") {
    lazy val expected = Pair ("A.splitAt(3)", (List ('A', 'B', 'C'), List ('D', 'E', 'F')  )  )
    lazy val obtained = ListExample () .splitAtExample
    assert (obtained == expected )
  }

  test ("should test A.indices") {
    lazy val expected = Pair ("A.indices", Range (0, 6 )  )
    lazy val obtained = ListExample () .indicesExample
    assert (obtained == expected )
  }

  test ("should test A.zipWithIndex") {
    lazy val expected = Pair ("A.zipWithIndex", List (('A', 0 ), ('B', 1 ), ('C', 2 ), ('D', 3 ), ('E', 4 ), ('F', 5 )  )  )
    lazy val obtained = ListExample () .zipWithIndexExample
    assert (obtained == expected )
  }

  test ("should test A.zip") {
    lazy val expected = Pair ("A.zip(B)", List (('A', 10 ), ('B', 20 ), ('C', 30 ), ('D', 40 ), ('E', 50 ), ('F', 60 )  )  )
    lazy val obtained = ListExample () .zipExample
    assert (obtained == expected )
  }

  test ("should test A.reverse") {
    lazy val expected = Pair ("A.reverse", List ('F', 'E', 'D', 'C', 'B', 'A')  )
    lazy val obtained = ListExample () .reverseExample
    assert (obtained == expected )
  }

  test ("should test A.+") {
    lazy val expected = Pair ("A.+:('X')", List ('X', 'A', 'B', 'C', 'D', 'E', 'F')  )
    lazy val obtained = ListExample () .prependedExample
    assert (obtained == expected )
  }

  test ("should test A.:") {
    lazy val expected = Pair ("A.:+('X')", List ('A', 'B', 'C', 'D', 'E', 'F', 'X')  )
    lazy val obtained = ListExample () .appendedExample
    assert (obtained == expected )
  }

  test ("should test A.++") {
    lazy val expected = Pair ("A.map(_.toInt).++(B)", List (65, 66, 67, 68, 69, 70, 10, 20, 30, 40, 50, 60 )  )
    lazy val obtained = ListExample () .concatExample
    assert (obtained == expected )
  }

  test ("should test A.span") {
    lazy val expected = Pair ("A.span(x -> not (x == 'D'))", (List ('A', 'B', 'C'), List ('D', 'E', 'F')  )  )
    lazy val obtained = ListExample () .spanExample
    assert (obtained == expected )
  }

  test ("should test A.map") {
    lazy val expected = Pair ("A.map(x -> x.toInt)", List (65, 66, 67, 68, 69, 70 )  )
    lazy val obtained = ListExample () .mapExample
    assert (obtained == expected )
  }

  test ("should test A.filter") {
    lazy val expected = Pair ("A.filter(x -> x.toInt % 2 == 0)", List ('B', 'D', 'F')  )
    lazy val obtained = ListExample () .filterExample
    assert (obtained == expected )
  }

  test ("should test B.fold") {
    lazy val expected = Pair ("B.fold(0)((a, b) -> a + b)", 210 )
    lazy val obtained = ListExample () .foldExample
    assert (obtained == expected )
  }

  test ("should test A.foldLeft") {
    lazy val expected = Pair ("A.foldLeft(Seq('*'))((list, elem) -> \"(\" + list + \" :+ \" + elem + \")\")", "((((((List(*) :+ A) :+ B) :+ C) :+ D) :+ E) :+ F)".toCharArray.toSeq )
    lazy val obtained = ListExample () .foldLeftExample
    assert (obtained == expected )
  }

  test ("should test A.foldRight") {
    lazy val expected = Pair ("A.foldRight(Seq('*'))((elem, list) -> \"(\" + elem + \" +: \" + list + \")\")", "(A +: (B +: (C +: (D +: (E +: (F +: List(*)))))))".toCharArray.toSeq )
    lazy val obtained = ListExample () .foldRightExample
    assert (obtained == expected )
  }

}
