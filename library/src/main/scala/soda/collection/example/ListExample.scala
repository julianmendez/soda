package soda.collection.example


trait Pair [T] {

  def name: String

  def result: T

}

case class Pair_ [T]  (name: String, result: T )  extends Pair [T]

trait ListExample {

  lazy val a: Seq [Char] = Seq ('A', 'B', 'C', 'D', 'E', 'F')
  lazy val b: Seq [Int] = Seq (10, 20, 30, 40, 50, 60 )

  lazy val a_example: Pair [Seq [Char]] =
    Pair_ ("a", a )

  lazy val b_example: Pair [Seq [Int]] =
    Pair_ ("b", b )

  lazy val take_example: Pair [Seq [Char]] =
    Pair_ ("a.take(3)", a.take (3 )  )

  lazy val takeRight_example: Pair [Seq [Char]] =
    Pair_ ("a.takeRight(3)", a.takeRight (3 )  )

  lazy val takeWhile_example: Pair [Seq [Char]] =
    Pair_ ("a.takeWhile(x -> not (x == 'E'))", a.takeWhile (x => ! (x == 'E')  )  )

  lazy val drop_example: Pair [Seq [Char]] =
    Pair_ ("a.drop(2)", a.drop (2 )  )

  lazy val dropRight_example: Pair [Seq [Char]] =
    Pair_ ("a.dropRight(2)", a.dropRight (2 )  )

  lazy val dropWhile_example: Pair [Seq [Char]] =
    Pair_ ("a.dropWhile(x -> not (x == 'E'))", a.dropWhile (x => ! (x == 'E')  )  )

  lazy val splitAt_example: Pair [(Seq [Char], Seq [Char]  )] =
    Pair_ ("a.splitAt(3)", a.splitAt (3 )  )

  lazy val indices_example: Pair [Range] =
    Pair_ ("a.indices", a.indices )

  lazy val zipWithIndex_example: Pair [Seq [(Char, Int )]] =
    Pair_ ("a.zipWithIndex", a.zipWithIndex )

  lazy val zip_example: Pair [Seq [(Char, Int )]] =
    Pair_ ("a.zip(b)", a.zip (b )  )

  lazy val reverse_example: Pair [Seq [Char]] =
    Pair_ ("a.reverse", a.reverse )

  /**
   * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
   */
  lazy val prepended_example: Pair [Seq [Char]] =
    Pair_ ("a.+:('X')", (a.+: ('X')  )  )

  /**
   * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
   */
  lazy val appended_example: Pair [Seq [Char]] =
    Pair_ ("a.:+('X')", (a.:+ ('X')  )  )

  lazy val concat_example: Pair [Seq [Int]] =
    Pair_ ("a.map(_.toInt).++(b)", a.map (_.toInt ) .++ (b )  )

  lazy val span_example: Pair [(Seq [Char], Seq [Char]  )] =
    Pair_ ("a.span(x -> not (x == 'D'))", a.span (x => ! (x == 'D')  )  )

  lazy val map_example: Pair [Seq [Int]] =
    Pair_ ("a.map(x -> x.toInt)", a.map (x => x.toInt )  )

  lazy val filter_example: Pair [Seq [Char]] =
    Pair_ ("a.filter(x -> x.toInt % 2 == 0)", a.filter (x => x.toInt % 2 == 0 )  )

  lazy val fold_example: Pair [Int] =
    Pair_ ("b.fold(0)((a, b) -> a + b)", b.fold (0 )  ((a, b ) => a + b )  )

  lazy val foldLeft_example: Pair [Seq [Char]] =
    Pair_ ("a.foldLeft(Seq('*'))((list, elem) -> \"(\" + list + \" :+ \" + elem + \")\")", a.foldLeft (Seq ('*')  )  ((list, elem ) => "(" + list + " :+ " + elem + ")")  )

  lazy val foldRight_example: Pair [Seq [Char]] =
    Pair_ ("a.foldRight(Seq('*'))((elem, list) -> \"(\" + elem + \" +: \" + list + \")\")", a.foldRight (Seq ('*')  )  ((elem, list ) => "(" + elem + " +: " + list + ")")  )

}

case class ListExample_ ()  extends ListExample
