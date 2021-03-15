package scopus.collection.example

case class ListExample() {

  lazy val A: Seq[Char] = Seq('A', 'B', 'C', 'D', 'E', 'F')
  lazy val B: Seq[Int] = Seq(10, 20, 30, 40, 50, 60)

  def aExample: (String, Seq[Char]) =
    ("A", A)

  def bExample: (String, Seq[Int]) =
    ("B", B)

  def takeExample: (String, Seq[Char]) =
    ("A.take(3)", A.take(3))

  def takeRightExample: (String, Seq[Char]) =
    ("A.takeRight(3)", A.takeRight(3))

  def takeWhileExample: (String, Seq[Char]) =
    ("A.takeWhile(x -> not (x == 'E'))", A.takeWhile(x => ! (x == 'E')))

  def dropExample: (String, Seq[Char]) =
    ("A.drop(2)", A.drop(2))

  def dropRightExample: (String, Seq[Char]) =
    ("A.dropRight(2)", A.dropRight(2))

  def dropWhileExample: (String, Seq[Char]) =
    ("A.dropWhile(x -> not (x == 'E'))", A.dropWhile(x => ! (x == 'E')))

  def splitAtExample: (String, (Seq[Char], Seq[Char])) =
    ("A.splitAt(3)", A.splitAt(3))

  def indicesExample: (String, Range) =
    ("A.indices", A.indices)

  def zipWithIndexExample: (String, Seq[(Char, Int)]) =
    ("A.zipWithIndex", A.zipWithIndex)

  def zipExample: (String, Seq[(Char, Int)]) =
    ("A.zip(B)", A.zip(B))

  def reverseExample: (String, Seq[Char]) =
    ("A.reverse", A.reverse)

  /**
   * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
   */
  def prependExample: (String, Seq[Char]) =
    ("'X' +: A", ('X' +: A))

  /**
   * A mnemonic for `+:` vs. `:+` is: the COLon goes on the COLlection side.
   */
  def appendExample: (String, Seq[Char]) =
    ("A :+ 'X'", (A :+ 'X'))

  def concatExample: (String, Seq[Int]) =
    ("A.map(_.toInt).++(B)", A.map(_.toInt).++(B))

  def spanExample: (String, (Seq[Char], Seq[Char])) =
    ("A.span(x -> not (x == 'D'))", A.span(x => ! (x == 'D')))

  def mapExample: (String, Seq[Int]) =
    ("A.map(x -> x.toInt)", A.map(x => x.toInt))

  def filterExample: (String, Seq[Char]) =
    ("A.filter(x -> x.toInt % 2 == 0)", A.filter(x => x.toInt % 2 == 0))

  def foldExample: (String, Int) =
    ("B.fold(0)((a, b) -> a + b)", B.fold(0)((a, b) => a + b))

  def foldLeftExample: (String, Seq[Char]) =
    ("A.foldLeft(Seq('*'))((list, elem) -> \"(\" + list + \" :+ \" + elem + \")\")", A.foldLeft(Seq('*'))((list, elem) => "(" + list + " :+ " + elem + ")"))

  def foldRightExample: (String, Seq[Char]) =
    ("A.foldRight(Seq('*'))((elem, list) -> \"(\" + elem + \" +: \" + list + \")\")", A.foldRight(Seq('*'))((elem, list) => "(" + elem + " +: " + list + ")"))

}
