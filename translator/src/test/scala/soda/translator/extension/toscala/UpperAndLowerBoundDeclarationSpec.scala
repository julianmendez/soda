package soda.translator.extension.toscala

case class UpperAndLowerBoundDeclarationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.parser.BlockProcessor_

  lazy val instance = BlockProcessor_ (MicroTranslatorToScala_ ()  )

  test ("should translate a single upper bound")
    {
      lazy val original = "  * BlackBox()" + "\n  extends " + "\n    AbstractBlackBox[A subtype AbstractInput]\n\nend\n"
      lazy val expected = "  case class BlackBox ()" + "\n  extends" + "\n    AbstractBlackBox [A <: AbstractInput]\n{\n\n}\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate multiple upper bounds")
    {
      lazy val original = "  * BlackBox()" + "\n  extends " + "\n    AbstractBlackBox[A subtype AbstractInput]\n    AbstractDevice[B subtype AbstractDeviceInput]\n\nend\n"
      lazy val expected = "  case class BlackBox ()" + "\n  extends" + "\n    AbstractBlackBox [A <: AbstractInput]\n    with AbstractDevice [B <: AbstractDeviceInput]\n{\n\n}\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate a single lower bound")
    {
      lazy val original = "  * BlackBox()" + "\n  extends " + "\n    AbstractBlackBox[A supertype (AbstractInput)]\n\nend\n"
      lazy val expected = "  case class BlackBox ()" + "\n  extends" + "\n    AbstractBlackBox [A >: (AbstractInput )]\n{\n\n}\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate multiple lower bounds")
    {
      lazy val original = "  * BlackBox()" + "\n  extends " + "\n    AbstractBlackBox[A supertype (AbstractInput)]" + "\n    AbstractDevice[B supertype (AbstractDeviceInput)]\n\nend\n"
      lazy val expected = "  case class BlackBox ()" + "\n  extends" + "\n    AbstractBlackBox [A >: (AbstractInput )]" + "\n    with AbstractDevice [B >: (AbstractDeviceInput )]\n{\n\n}\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

}
