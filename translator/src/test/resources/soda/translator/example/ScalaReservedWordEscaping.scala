package soda.translator.example


trait ScalaReservedWordEscaping {

  lazy val __soda__var = "var"

  lazy val __soda__val = 1

  def __soda__def [A, B]  (key: A, value: B ): Pair [A, B] = Pair_ (key, value )

  def __soda__while [A, B]  (seq: Seq [A], cond: A => Boolean, __soda__do: A => B ): Seq [B] =
    seq.takeWhile (cond ) .map (__soda__do )

  lazy val __soda__protected = "protected"

  lazy val __soda__private = "private"

  def f (x: Int, y: Int ): Int = x + y

  lazy val cons: (Int, Int ) => Int = (x, y ) => f (x, y )
}

trait Pair [A, B] {

  def key: A

  def value: B
}

case class Pair_ [A, B]  (key: A, value: B ) extends Pair [A, B]
