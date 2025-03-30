

trait NonNegative
{

  def   v : Int

  lazy val invariant : Boolean =
    v >= 0

  lazy val check : Option [NonNegative] =
    if ( invariant
    ) Some (this)
    else None

}

case class NonNegative_ (v : Int) extends NonNegative

object NonNegative {
  def mk (v : Int) : NonNegative =
    NonNegative_ (v)
}

trait FiboExampleInSoda
{



  private def _plus (a : NonNegative) (b : NonNegative) : NonNegative =
    NonNegative .mk (a .v + b .v)

  private def _monus1 (a : NonNegative) : NonNegative =
    if ( a .v > 0
    ) NonNegative .mk (a .v - 1)
    else a

  private def _tailrec (m : NonNegative) (a : NonNegative) (b : NonNegative) : NonNegative =
    if ( m .v == 0 ) a
    else if ( m .v == 1 ) b
    else _tailrec (_monus1 (m) ) (b) (_plus (a) (b) )

  private def _apply (n : NonNegative) : NonNegative =
    _tailrec (n) (NonNegative .mk (0) ) (NonNegative .mk (1) )

  def apply (n : Int) : Int =
    NonNegative .mk (n) .check match  {
      case Some (non_negative) => (_apply (non_negative) ) .v
      case None => -1
    }

}

case class FiboExampleInSoda_ () extends FiboExampleInSoda

object FiboExampleInSoda {
  def mk : FiboExampleInSoda =
    FiboExampleInSoda_ ()
}
