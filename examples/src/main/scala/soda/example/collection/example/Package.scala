package soda.collection.example

/*
 * This package contains examples for Soda.
 */

trait Package

trait Pair [A ]
{

  def   name : String
  def   result : A

}

case class Pair_ [A] (name : String, result : A) extends Pair [A]

object Pair {
  def mk [A] (name : String) (result : A) : Pair [A] =
    Pair_ [A] (name, result)
}

trait ListExample
{



  private def _mk_Pair [A ] (name : String) (result : A) : Pair [A] =
    Pair_ (name, result)

  lazy val a : Seq [Char] = Seq ('A', 'B', 'C', 'D', 'E', 'F')

  lazy val b : Seq [Int] = Seq (10 , 20 , 30 , 40 , 50 , 60)

  lazy val a_example : Pair [Seq [Char] ] =
    _mk_Pair ("a") (a)

  lazy val b_example : Pair [Seq [Int] ] =
    _mk_Pair ("b") (b)

  lazy val take_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .take (3)") (a .take (3) )

  lazy val takeRight_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .takeRight (3)") (a .takeRight (3) )

  lazy val takeWhile_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .takeWhile (lambda x --> not (x == 'E') )") (
      a .takeWhile ( x => ! (x == 'E') ) )

  lazy val drop_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .drop (2)") (a .drop (2) )

  lazy val dropRight_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .dropRight (2)") (a .dropRight (2))

  lazy val dropWhile_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .dropWhile (lambda x --> not (x == 'E'))") (
      a .dropWhile ( x => ! (x == 'E') ) )

  lazy val splitAt_example : Pair [Tuple2 [Seq [Char] , Seq [Char] ] ] =
    _mk_Pair ("a .splitAt (3)") (a .splitAt (3) )

  lazy val indices_example : Pair [Range] =
    _mk_Pair ("a .indices") (a .indices)

  lazy val zipWithIndex_example : Pair [Seq [Tuple2 [Char, Int] ] ] =
    _mk_Pair ("a .zipWithIndex") (a .zipWithIndex)

  lazy val zip_example : Pair [Seq [Tuple2 [Char, Int] ] ] =
    _mk_Pair ("a .zip (b)") (a .zip (b) )

  lazy val reverse_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .reverse") (a .reverse)

  /**
   * A mnemonic for `+:` vs. `:+` is that the COLon goes on the COLlection side.
   */
  lazy val prepended_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .+: ('X')") ( (a .+: ('X') ) )

  /**
   * A mnemonic for `+:` vs. `:+` is that the COLon goes on the COLlection side.
   */
  lazy val appended_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .:+ ('X')") ( (a .:+ ('X') ) )

  lazy val concat_example : Pair [Seq [Int] ] =
    _mk_Pair ("a .map (lambda x --> x .toInt) .++ (b)") (a .map ( x => x .toInt) .++ (b)
    )

  lazy val span_example : Pair [ Tuple2 [Seq [Char] , Seq [Char] ] ] =
    _mk_Pair ("a .span (lambda x --> not (x == 'D') )") (a .span ( x => ! (x == 'D') )
    )

  lazy val map_example : Pair [Seq [Int] ] =
    _mk_Pair ("a .map (lambda x --> x .toInt)") (a .map ( x => x .toInt) )

  lazy val filter_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .filter (lambda x --> x .toInt % 2 == 0)") (
      a .filter ( x => x .toInt % 2 == 0) )

  lazy val fold_example : Pair [Int] =
    _mk_Pair ("b .fold(0) (lambda (a , b) --> a + b)") (b .fold (0) ( (a , b) => a + b) )

  lazy val foldLeft_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .foldLeft (Seq ('*') )" +
      " (lambda (list , elem) --> \"(\" + list + \" :+ \" + elem + \")\")") (
      a .foldLeft (Seq ('*') ) ( (list , elem) => "(" + list + " :+ " + elem + ")") )

  lazy val foldRight_example : Pair [Seq [Char] ] =
    _mk_Pair ("a .foldRight (Seq ('*') )" +
      " (lambda (elem , list) --> \"(\" + elem + \" +: \" + list + \")\")") (
      a.foldRight (Seq ('*') ) ( (elem , list) => "(" + elem + " +: " + list + ")") )

}

case class ListExample_ () extends ListExample

object ListExample {
  def mk : ListExample =
    ListExample_ ()
}


trait Main
{



  lazy val e = ListExample_ ()

  lazy val all_examples =
    Seq (
      e .a_example,
      e .b_example,
      e .take_example,
      e .takeRight_example,
      e .takeWhile_example,
      e .drop_example,
      e .dropRight_example,
      e .dropWhile_example,
      e .splitAt_example,
      e .indices_example,
      e .zipWithIndex_example,
      e .zip_example,
      e .reverse_example,
      e .prepended_example,
      e .appended_example,
      e .concat_example,
      e .span_example,
      e .map_example,
      e .filter_example,
      e .fold_example,
      e .foldLeft_example,
      e .foldRight_example
    )
    .map ( pair => pair .name + " = " + pair .result .toString)
    .mkString ("\n")

  def main (arguments : Array [String] ) : Unit =
    println (all_examples)

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main {
  def mk : Main =
    Main_ ()
}

