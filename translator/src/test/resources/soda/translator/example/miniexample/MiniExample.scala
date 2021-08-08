package soda.translator.example.miniexample


trait MiniExample extends soda.lib.Recursive {

  def run () =
    range (50 )
      .map (x => print (" " + (2 * x + 1 )  ) )
}

case class MiniExample_ () extends MiniExample

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

trait MainClass {
  def main (args: Array [String]  ): Unit =
    MiniExample_ () .run ()
}

case class Main () extends MainClass
