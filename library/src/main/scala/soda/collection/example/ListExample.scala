package soda.collection.example


case class Pair [T]  (name: String, result: T )

trait ListExample {

  lazy val a: Seq [Char] = Seq ('A', 'B', 'C', 'D', 'E', 'F')
  lazy val b: Seq [Int] = Seq (10, 20, 30, 40, 50, 60 )

  lazy val aExample: Pair [Seq [Char]] =
    Pair ("a", a )

  lazy val bExample: Pair [Seq [Int]] =
    Pair ("b", b )

  lazy val takeExample: Pair [Seq [Char]] =
    Pair ("a.take(3)", a.take (3 )  )

  lazy val takeRightExample: Pair [Seq [Char]] =
    Pair ("a.takeRight(3)", a.takeRight (3 )  )

  lazy val takeWhileExample: Pair [Seq [Char]] =
    Pair ("a.takeWhile(x -> not (x == 'E'))", a.takeWhile (x => ! (x == 'E')  )  )

  lazy val dropExample: Pair [Seq [Char]] =
    Pair ("a.drop(2)", a.drop (2 )  )

  lazy val dropRightExample: Pair [Seq [Char]] =
    Pair ("a.dropRight(2)", a.dropRight (2 )  )

  lazy val dropWhileExample: Pair [Seq [Char]] =
    Pair ("a.dropWhile(x -> not (x == 'E'))", a.dropWhile (x => ! (x == 'E')  )  )

  lazy val splitAtExample: Pair [(Seq [Char], Seq [Char]  )] =
    Pair ("a.splitAt(3)", a.splitAt (3 )  )

  lazy val indicesExample: Pair [Range] =
    Pair ("a.indices", a.indices )

  lazy val zipWithIndexExample: Pair [Seq [(Char, Int )]] =
    Pair ("a.zipWithIndex", a.zipWithIndex )

  lazy val zipExample: Pair [Seq [(Char, Int )]] =
    Pair ("a.zip(b)", a.zip (b )  )

  lazy val reverseExample: Pair [Seq [Char]] =
    Pair ("a.reverse", a.reverse )

  /**
   * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
   */
  lazy val prependedExample: Pair [Seq [Char]] =
    Pair ("a.+:('X')", (a.+: ('X')  )  )

  /**
   * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
   */
  lazy val appendedExample: Pair [Seq [Char]] =
    Pair ("a.:+('X')", (a.:+ ('X')  )  )

  lazy val concatExample: Pair [Seq [Int]] =
    Pair ("a.map(_.toInt).++(b)", a.map (_.toInt ) .++ (b )  )

  lazy val spanExample: Pair [(Seq [Char], Seq [Char]  )] =
    Pair ("a.span(x -> not (x == 'D'))", a.span (x => ! (x == 'D')  )  )

  lazy val mapExample: Pair [Seq [Int]] =
    Pair ("a.map(x -> x.toInt)", a.map (x => x.toInt )  )

  lazy val filterExample: Pair [Seq [Char]] =
    Pair ("a.filter(x -> x.toInt % 2 == 0)", a.filter (x => x.toInt % 2 == 0 )  )

  lazy val foldExample: Pair [Int] =
    Pair ("b.fold(0)((a, b) -> a + b)", b.fold (0 )  ((a, b ) => a + b )  )

  lazy val foldLeftExample: Pair [Seq [Char]] =
    Pair ("a.foldLeft(Seq('*'))((list, elem) -> \"(\" + list + \" :+ \" + elem + \")\")", a.foldLeft (Seq ('*')  )  ((list, elem ) => "(" + list + " :+ " + elem + ")")  )

  lazy val foldRightExample: Pair [Seq [Char]] =
    Pair ("a.foldRight(Seq('*'))((elem, list) -> \"(\" + elem + \" +: \" + list + \")\")", a.foldRight (Seq ('*')  )  ((elem, list ) => "(" + elem + " +: " + list + ")")  )
}

case class ListExampleImpl () extends ListExample
