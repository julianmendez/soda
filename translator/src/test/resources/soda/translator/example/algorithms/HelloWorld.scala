package soda.example.algorithms

trait Main
{

  def main (arguments: Array [String]  ): Unit =
    println ("Hello world!")

}

case class Main_ ()
  extends Main
{

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}

