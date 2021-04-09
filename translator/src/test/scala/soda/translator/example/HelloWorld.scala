package soda.translator.example


case class Main () {

  def main (args: Array [String]  ) =
    println ("Hello world!")

}

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

