package soda.collection.example


case class ListExampleSpec () extends org.scalatest.funsuite.AnyFunSuite {

  test ("should test a") {
    lazy val expected = Pair ("a", List ('A', 'B', 'C', 'D', 'E', 'F')  )
    lazy val obtained = ListExampleImpl () .aExample

    assert (obtained == expected )
  }

  test ("should test b") {
    lazy val expected = Pair ("b", List (10, 20, 30, 40, 50, 60 )  )
    lazy val obtained = ListExampleImpl () .bExample

    assert (obtained == expected )
  }

  test ("should test a.take") {
    lazy val expected = Pair ("a.take(3)", List ('A', 'B', 'C')  )
    lazy val obtained = ListExampleImpl () .takeExample

    assert (obtained == expected )
  }

  test ("should test a.takeRight") {
    lazy val expected = Pair ("a.takeRight(3)", List ('D', 'E', 'F')  )
    lazy val obtained = ListExampleImpl () .takeRightExample

    assert (obtained == expected )
  }

  test ("should test a.takeWhile") {
    lazy val expected = Pair ("a.takeWhile(x -> not (x == 'E'))", List ('A', 'B', 'C', 'D')  )
    lazy val obtained = ListExampleImpl () .takeWhileExample

    assert (obtained == expected )
  }

  test ("should test a.drop") {
    lazy val expected = Pair ("a.drop(2)", List ('C', 'D', 'E', 'F')  )
    lazy val obtained = ListExampleImpl () .dropExample

    assert (obtained == expected )
  }

  test ("should test a.dropRight") {
    lazy val expected = Pair ("a.dropRight(2)", List ('A', 'B', 'C', 'D')  )
    lazy val obtained = ListExampleImpl () .dropRightExample

    assert (obtained == expected )
  }

  test ("should test a.dropWhile") {
    lazy val expected = Pair ("a.dropWhile(x -> not (x == 'E'))", List ('E', 'F')  )
    lazy val obtained = ListExampleImpl () .dropWhileExample

    assert (obtained == expected )
  }

  test ("should test a.splitAt") {
    lazy val expected = Pair ("a.splitAt(3)", (List ('A', 'B', 'C'), List ('D', 'E', 'F')  )  )
    lazy val obtained = ListExampleImpl () .splitAtExample

    assert (obtained == expected )
  }

  test ("should test a.indices") {
    lazy val expected = Pair ("a.indices", Range (0, 6 )  )
    lazy val obtained = ListExampleImpl () .indicesExample

    assert (obtained == expected )
  }

  test ("should test a.zipWithIndex") {
    lazy val expected = Pair ("a.zipWithIndex", List (('A', 0 ), ('B', 1 ), ('C', 2 ), ('D', 3 ), ('E', 4 ), ('F', 5 )  )  )
    lazy val obtained = ListExampleImpl () .zipWithIndexExample

    assert (obtained == expected )
  }

  test ("should test a.zip") {
    lazy val expected = Pair ("a.zip(b)", List (('A', 10 ), ('B', 20 ), ('C', 30 ), ('D', 40 ), ('E', 50 ), ('F', 60 )  )  )
    lazy val obtained = ListExampleImpl () .zipExample

    assert (obtained == expected )
  }

  test ("should test a.reverse") {
    lazy val expected = Pair ("a.reverse", List ('F', 'E', 'D', 'C', 'B', 'A')  )
    lazy val obtained = ListExampleImpl () .reverseExample

    assert (obtained == expected )
  }

  test ("should test a.+") {
    lazy val expected = Pair ("a.+:('X')", List ('X', 'A', 'B', 'C', 'D', 'E', 'F')  )
    lazy val obtained = ListExampleImpl () .prependedExample

    assert (obtained == expected )
  }

  test ("should test a.:") {
    lazy val expected = Pair ("a.:+('X')", List ('A', 'B', 'C', 'D', 'E', 'F', 'X')  )
    lazy val obtained = ListExampleImpl () .appendedExample

    assert (obtained == expected )
  }

  test ("should test a.++") {
    lazy val expected = Pair ("a.map(_.toInt).++(b)", List (65, 66, 67, 68, 69, 70, 10, 20, 30, 40, 50, 60 )  )
    lazy val obtained = ListExampleImpl () .concatExample

    assert (obtained == expected )
  }

  test ("should test a.span") {
    lazy val expected = Pair ("a.span(x -> not (x == 'D'))", (List ('A', 'B', 'C'), List ('D', 'E', 'F')  )  )
    lazy val obtained = ListExampleImpl () .spanExample

    assert (obtained == expected )
  }

  test ("should test a.map") {
    lazy val expected = Pair ("a.map(x -> x.toInt)", List (65, 66, 67, 68, 69, 70 )  )
    lazy val obtained = ListExampleImpl () .mapExample

    assert (obtained == expected )
  }

  test ("should test a.filter") {
    lazy val expected = Pair ("a.filter(x -> x.toInt % 2 == 0)", List ('B', 'D', 'F')  )
    lazy val obtained = ListExampleImpl () .filterExample

    assert (obtained == expected )
  }

  test ("should test b.fold") {
    lazy val expected = Pair ("b.fold(0)((a, b) -> a + b)", 210 )
    lazy val obtained = ListExampleImpl () .foldExample

    assert (obtained == expected )
  }

  test ("should test a.foldLeft") {
    lazy val expected = Pair ("a.foldLeft(Seq('*'))((list, elem) -> \"(\" + list + \" :+ \" + elem + \")\")", "((((((List(*) :+ A) :+ B) :+ C) :+ D) :+ E) :+ F)".toCharArray.toSeq )
    lazy val obtained = ListExampleImpl () .foldLeftExample

    assert (obtained == expected )
  }

  test ("should test a.foldRight") {
    lazy val expected = Pair ("a.foldRight(Seq('*'))((elem, list) -> \"(\" + elem + \" +: \" + list + \")\")", "(A +: (B +: (C +: (D +: (E +: (F +: List(*)))))))".toCharArray.toSeq )
    lazy val obtained = ListExampleImpl () .foldRightExample

    assert (obtained == expected )
  }
}
