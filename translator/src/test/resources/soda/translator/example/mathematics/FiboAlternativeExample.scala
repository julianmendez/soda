
trait NonNegativeMod
{

  def invariant (v : Int) : Boolean =
    v >= 0

  def mko (v : Int) : Option [Int] =
    if ( invariant (v)
    ) Some (v)
    else None

  private def _plus (v : Int) (b : Option [Int] ) : Option [Int] =
    b match  {
      case Some (w) => mko (v + w)
      case None => None
    }

  def plus (a : Option [Int] ) (b : Option [Int] ) : Option [Int] =
    a match  {
      case Some (v) => _plus (v) (b)
      case None => None
    }

  private def _minus1 (v : Int) : Option [Int] =
    if ( v > 0
    ) Option [Int] (v - 1)
    else None

  def minus1 (a : Option [Int]) : Option [Int] =
    a match  {
      case Some (v) => _minus1 (v)
      case None => None
    }

}

case class NonNegativeMod_ () extends NonNegativeMod

object NonNegativeMod {
  def mk : NonNegativeMod =
    NonNegativeMod_ ()
}

trait FiboAlternativeExampleInSoda
{



  private lazy val _mm = NonNegativeMod .mk

  private lazy val _zero = _mm .mko (0)

  private lazy val _one = _mm .mko (1)

  private def _tailrec (m : Option [Int] ) (a : Option [Int] ) (b : Option [Int] ) : Option [Int] =
    if ( m == _zero ) a
    else if ( m == _one ) b
    else _tailrec (_mm .minus1 (m) ) (b) (_mm .plus (a) (b) )

  private def _apply (n : Option [Int] ) : Option [Int] =
    _tailrec (n) (_zero) (_one)

  def apply (n : Int) : Int =
    _apply (_mm .mko (n) ) match  {
      case Some (v) => v
      case None => -1
    }

}

case class FiboAlternativeExampleInSoda_ () extends FiboAlternativeExampleInSoda

object FiboAlternativeExampleInSoda {
  def mk : FiboAlternativeExampleInSoda =
    FiboAlternativeExampleInSoda_ ()
}
