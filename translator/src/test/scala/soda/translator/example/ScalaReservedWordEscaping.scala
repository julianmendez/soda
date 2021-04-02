package soda.translator.example

case class ScalaReservedWordEscaping (  ) {

  lazy val __scopus__var = "var"

  lazy val __scopus__val = 1

  def __scopus__def [ A , B ]  ( key: A , value: B ) : ( A , B ) = ( key , value )

  def __scopus__while [ A , B ]  ( seq: Seq [ A ]  , cond: A => Boolean , __scopus__do : A => B ) =
    seq.takeWhile ( cond ) .map ( __scopus__do )

  lazy val __scopus__protected = "protected"
  lazy val __scopus__private = "private"

}
