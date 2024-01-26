trait Main
{

  def main (arguments : Array [String] ) : Unit =
    println ("Hello world!")

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main {
  def mk : Main =
    Main_ ()
}
