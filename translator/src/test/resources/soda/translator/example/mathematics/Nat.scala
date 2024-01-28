/*
 * This is an implementation of Nat in Scala for Soda.
 */

sealed trait Nat {
  def v : Int
}
case class _Nat_ (v : Int) extends Nat {
}
object Nat {
  def mk (v : Int) : Nat =
    if (v <= 0) Zero_ else _Nat_ (v)
}
case object Zero_ extends Nat {
  def v : Int = 0
}
object Succ_ {
  def apply (n : Nat) : Nat =
    if (n.v <= 0) Nat.mk (1) else Nat.mk (n.v + 1)
  def unapply (n : Nat) : Option [Nat] =
    if (n.v <= 0) None else Some (Nat.mk (n.v - 1) )
}
