package soda.translator.example.miniexample


case class MiniExample () extends soda.lib.Recursive {

  def run () =
    range (50 )
      .map (x => print (" " + (2 * x + 1 )  ) )

}

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

case class Main () {
  def main (args: Array [String]  ): Unit =
    MiniExample () .run ()
}
