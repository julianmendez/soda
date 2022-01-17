package soda.translator.extension.toscala

case class UpperAndLowerBoundDeclarationSpec ()
  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.translator.parser.BlockProcessor_

  lazy val instance = BlockProcessor_ (MicroTranslatorToScala_ ()  )

  test ("should translate a single upper bound")
    {
      lazy val original = "  * BlackBox() extends AbstractBlackBox[A subtype AbstractInput]\n"
      lazy val expected = "  case class BlackBox () extends AbstractBlackBox [A <: AbstractInput]\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate multiple upper bounds")
    {
      lazy val original = "  * BlackBox() extends AbstractBlackBox[A subtype AbstractInput] with AbstractDevice[B subtype AbstractDeviceInput]\n"
      lazy val expected = "  case class BlackBox () extends AbstractBlackBox [A <: AbstractInput] with AbstractDevice [B <: AbstractDeviceInput]\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate a single lower bound")
    {
      lazy val original = "  * BlackBox() extends AbstractBlackBox[A supertype (AbstractInput)]\n"
      lazy val expected = "  case class BlackBox () extends AbstractBlackBox [A >: (AbstractInput )]\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate multiple lower bounds")
    {
      lazy val original = "  * BlackBox() extends AbstractBlackBox[A supertype (AbstractInput)] with AbstractDevice[B supertype (AbstractDeviceInput)]\n"
      lazy val expected = "  case class BlackBox () extends AbstractBlackBox [A >: (AbstractInput )] with AbstractDevice [B >: (AbstractDeviceInput )]\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

}
