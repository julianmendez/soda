trait MyPair [A , B ]
{

  def   key : A
  def   value : B

}

case class MyPair_ [A, B] (key : A, value : B) extends MyPair [A, B]

trait ScalaReservedWordEscaping
{

  private lazy val __soda__var = "var"

  private lazy val __soda__val = 1

  def g [A , B ] (key : A) (value : B) : MyPair [A, B] = MyPair_ (key , value)

  private def __soda__while [A , B ] (seq : Seq [A] ) (cond : A => Boolean) (funct : A => B) : Seq [B] =
    seq .takeWhile (cond) .map (funct)

  private lazy val __soda__protected = "protected"

  private lazy val __soda__private = "private"

  def f (x : Int) (y : Int) : Int = x + y

  lazy val cons : Int => Int => Int =
     x =>
       y =>
        f (x) (y)

}

case class ScalaReservedWordEscaping_ () extends ScalaReservedWordEscaping
