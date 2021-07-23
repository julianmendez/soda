package soda.translator.example


trait MainClass {

  def main (args: Array [String]  ): Unit =
    println ("Hello world!")
}

case class Main () extends MainClass

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

