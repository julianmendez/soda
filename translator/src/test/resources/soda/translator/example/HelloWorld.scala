package soda.example.algorithms

trait MainClass
{

  def main (arguments: Array [String]  ): Unit =
    println ("Hello world!")

}

case class Main ()
  extends
    MainClass
{

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main ().main (args)
}

