package soda.example


trait Parameter {

  def name: String
}

trait PatternMatching {

  def get_value (p: Parameter ): Int =
    p  match {
      case Singleton (x ) => x
      case Pair (x, y ) => (x + y ) / 2
      case Triplet (x, y, z ) => (x + y + z ) / 3
      case otherwise => 0
    }

  def get_type_name (p: Parameter ): String =
    p  match {
      case x: Singleton => x.name + "(x)"
      case x: Pair => x.name + "(x, y)"
      case x: Triplet => x.name + "(x, y, z)"
      case otherwise => ""
    }
}

case class PatternMatching_ ()  extends PatternMatching

case class Singleton (x: Int )  extends Parameter {

  lazy val name = "singleton"
}

case class Pair (x: Int, y: Int )  extends Parameter {

  lazy val name = "pair"
}

case class Triplet (x: Int, y: Int, z: Int )  extends Parameter {

  lazy val name = "triplet"
}
