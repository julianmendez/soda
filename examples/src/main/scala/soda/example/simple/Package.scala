package soda.example.simple



trait Color
{

  def   cyan : Int
  def   magenta : Int
  def   yellow : Int

}

case class Color_ (cyan : Int, magenta : Int, yellow : Int) extends Color

object Color {
  def mk (cyan : Int) (magenta : Int) (yellow : Int) : Color =
    Color_ (cyan, magenta, yellow)
}

trait Palette
{



  lazy val cyan = Color .mk (100) (0) (0)

  lazy val yellow = Color .mk (0) (0) (100)

  def plus (a : Color) (b : Color) : Color =
    Color .mk (a .cyan + b .cyan) (a .magenta + b .magenta) (a .yellow + b .yellow)

  lazy val green = plus (cyan) (yellow)

}

case class Palette_ () extends Palette

object Palette {
  def mk : Palette =
    Palette_ ()
}

trait Main
{



  def main (arguments : Array [String] ) : Unit =
    print (Palette .mk .green)

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main {
  def mk : Main =
    Main_ ()
}

