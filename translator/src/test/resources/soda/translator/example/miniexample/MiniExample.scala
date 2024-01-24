trait MiniExample
{

  import   soda.lib.Range_

  private lazy val _range = Range_ ()

  def run () : Seq [Unit] =
    _range.apply (50)
      .map ( x => print (" " + (2 * x + 1) ) )

}

case class MiniExample_ () extends MiniExample

object MiniExample { def mk   : MiniExample  = MiniExample_  () }

trait Main
{

  def main (arguments : Array [String] ) : Unit =
    MiniExample_ () .run ()

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main { def mk   : Main  = Main_  () }
