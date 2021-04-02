package soda.translator.example

case class ScalaReservedWordEscaping (  ) {

  lazy val __soda__var = "var"

  lazy val __soda__val = 1

  def __soda__def [ A , B ]  ( key: A , value: B ) : ( A , B ) = ( key , value )

  def __soda__while [ A , B ]  ( seq: Seq [ A ]  , cond: A => Boolean , __soda__do : A => B ) =
    seq.takeWhile ( cond ) .map ( __soda__do )

  lazy val __soda__protected = "protected"
  lazy val __soda__private = "private"

}
