trait Singleton
  extends
    Parameter
{

  def   x : Int

  lazy val name = "singleton"

}

case class Singleton_ (x : Int) extends Singleton

object Singleton {
  def mk (x : Int) : Singleton =
    Singleton_ (x)
}

trait Pair
  extends
    Parameter
{

  def   x : Int
  def   y : Int

  lazy val name = "pair"

}

case class Pair_ (x : Int, y : Int) extends Pair

object Pair {
  def mk (x : Int) (y : Int) : Pair =
    Pair_ (x, y)
}

trait Triplet
  extends
    Parameter
{

  def   x : Int
  def   y : Int
  def   z : Int

  lazy val name = "triplet"

}

case class Triplet_ (x : Int, y : Int, z : Int) extends Triplet

object Triplet {
  def mk (x : Int) (y : Int) (z : Int) : Triplet =
    Triplet_ (x, y, z)
}

trait Parameter
{

  def   name : String

}

case class Parameter_ (name : String) extends Parameter

object Parameter {
  def mk (name : String) : Parameter =
    Parameter_ (name)
}

trait PatternMatching
{

  def get_value (p : Parameter) : Int =
    p match  {
      case Singleton_ (x) => x
      case Pair_ (x, y) => (x + y) / 2
      case Triplet_ (x, y, z) => (x + y + z) / 3
      case otherwise => 0
    }

  def get_type_name (p : Parameter) : String =
    p match  {
      case Singleton_ (x) => (Singleton_ (x) ) .name + " (x)"
      case Pair_ (x, y) => (Pair_ (x, y) ) .name + " (x) (y)"
      case Triplet_ (x, y, z) => (Triplet_ (x, y, z) ) .name + " (x) (y) (z)"
      case otherwise => ""
    }

}

case class PatternMatching_ () extends PatternMatching

object PatternMatching {
  def mk : PatternMatching =
    PatternMatching_ ()
}
