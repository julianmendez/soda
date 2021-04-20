package soda.collection.example

case class Pair [T]  (name: String, result: T )

case class ListExample () {

  lazy val A: Seq [Char] = Seq ('A', 'B', 'C', 'D', 'E', 'F')
  lazy val B: Seq [Int] = Seq (10, 20, 30, 40, 50, 60 )

  lazy val aExample: Pair [Seq [Char]] =
    Pair ("A", A )

  lazy val bExample: Pair [Seq [Int]] =
    Pair ("B", B )

  lazy val takeExample: Pair [Seq [Char]] =
    Pair ("A.take(3)", A.take (3 )  )

  lazy val takeRightExample: Pair [Seq [Char]] =
    Pair ("A.takeRight(3)", A.takeRight (3 )  )

  lazy val takeWhileExample: Pair [Seq [Char]] =
    Pair ("A.takeWhile(x -> not (x == 'E'))", A.takeWhile (x => ! (x == 'E')  )  )

  lazy val dropExample: Pair [Seq [Char]] =
    Pair ("A.drop(2)", A.drop (2 )  )

  lazy val dropRightExample: Pair [Seq [Char]] =
    Pair ("A.dropRight(2)", A.dropRight (2 )  )

  lazy val dropWhileExample: Pair [Seq [Char]] =
    Pair ("A.dropWhile(x -> not (x == 'E'))", A.dropWhile (x => ! (x == 'E')  )  )

  def splitAtExample: Pair [(Seq [Char], Seq [Char]  )] =
    Pair ("A.splitAt(3)", A.splitAt (3 )  )

  lazy val indicesExample: Pair [Range] =
    Pair ("A.indices", A.indices )

  def zipWithIndexExample: Pair [Seq [(Char, Int )]] =
    Pair ("A.zipWithIndex", A.zipWithIndex )

  def zipExample: Pair [Seq [(Char, Int )]] =
    Pair ("A.zip(B)", A.zip (B )  )

  lazy val reverseExample: Pair [Seq [Char]] =
    Pair ("A.reverse", A.reverse )

  /**
   * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
   */
  lazy val prependedExample: Pair [Seq [Char]] =
    Pair ("A.+:('X')", (A.+: ('X')  )  )

  /**
   * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
   */
  lazy val appendedExample: Pair [Seq [Char]] =
    Pair ("A.:+('X')", (A.:+ ('X')  )  )

  lazy val concatExample: Pair [Seq [Int]] =
    Pair ("A.map(_.toInt).++(B)", A.map (_.toInt ) .++ (B )  )

  def spanExample: Pair [(Seq [Char], Seq [Char]  )] =
    Pair ("A.span(x -> not (x == 'D'))", A.span (x => ! (x == 'D')  )  )

  lazy val mapExample: Pair [Seq [Int]] =
    Pair ("A.map(x -> x.toInt)", A.map (x => x.toInt )  )

  lazy val filterExample: Pair [Seq [Char]] =
    Pair ("A.filter(x -> x.toInt % 2 == 0)", A.filter (x => x.toInt % 2 == 0 )  )

  lazy val foldExample: Pair [Int] =
    Pair ("B.fold(0)((a, b) -> a + b)", B.fold (0 )  ((a, b ) => a + b )  )

  lazy val foldLeftExample: Pair [Seq [Char]] =
    Pair ("A.foldLeft(Seq('*'))((list, elem) -> \"(\" + list + \" :+ \" + elem + \")\")", A.foldLeft (Seq ('*')  )  ((list, elem ) => "(" + list + " :+ " + elem + ")")  )

  lazy val foldRightExample: Pair [Seq [Char]] =
    Pair ("A.foldRight(Seq('*'))((elem, list) -> \"(\" + elem + \" +: \" + list + \")\")", A.foldRight (Seq ('*')  )  ((elem, list ) => "(" + elem + " +: " + list + ")")  )

}
