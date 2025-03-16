package soda.example.miniexample

/*
 * This package contains mini-examples.
 */

trait MiniExample
{



  import   soda.lib.Range

  private lazy val _range = Range .mk

  def run () : Seq [Unit] =
    _range.apply (50)
      .map ( x => print (" " + (2 * x + 1) ) )

}

case class MiniExample_ () extends MiniExample

object MiniExample {
  def mk : MiniExample =
    MiniExample_ ()
}

trait Main
{



  def main (arguments : Array [String] ) : Unit =
    MiniExample .mk .run ()

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main {
  def mk : Main =
    Main_ ()
}

