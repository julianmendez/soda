package soda.example.algorithms

trait Parameter
{

  def   name: String

}

case class Parameter_ (name: String) extends Parameter

trait PatternMatching
{

  def get_value (p: Parameter ): Int =
    p match  {
      case Singleton_ (x ) => x
      case Pair_ (x, y ) => (x + y ) / 2
      case Triplet_ (x, y, z ) => (x + y + z ) / 3
      case otherwise => 0
    }

  def get_type_name (p: Parameter ): String =
    p match  {
      case x: Singleton => x.name + "(x)"
      case x: Pair => x.name + "(x, y)"
      case x: Triplet => x.name + "(x, y, z)"
      case otherwise => ""
    }

}

case class PatternMatching_ () extends PatternMatching

trait Singleton
  extends
    Parameter
{

  def   x: Int

  lazy val name = "singleton"

}

case class Singleton_ (x: Int) extends Singleton

trait Pair
  extends
    Parameter
{

  def   x: Int
  def   y: Int

  lazy val name = "pair"

}

case class Pair_ (x: Int, y: Int) extends Pair

trait Triplet
  extends
    Parameter
{

  def   x: Int
  def   y: Int
  def   z: Int

  lazy val name = "triplet"

}

case class Triplet_ (x: Int, y: Int, z: Int) extends Triplet
