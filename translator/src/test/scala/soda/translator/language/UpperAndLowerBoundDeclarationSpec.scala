package soda.translator.language

import org.scalatest.funsuite.AnyFunSuite


case class UpperAndLowerBoundDeclarationSpec () extends AnyFunSuite {

  test ("should translate a single upper bound") {
    lazy val original = "  * BlackBox() extends AbstractBlackBox[A subof AbstractInput]\n"

    lazy val expected = "  case class BlackBox () extends AbstractBlackBox [A <: AbstractInput]\n"

    lazy val obtained = MicroTranslator () .translate_program (original )
    assert (obtained == expected )
  }


  test ("should translate multiple upper bounds") {
    lazy val original = "  * BlackBox() extends AbstractBlackBox[A subof AbstractInput] with AbstractDevice[B subof AbstractDeviceInput]\n"

    lazy val expected = "  case class BlackBox () extends AbstractBlackBox [A <: AbstractInput] with AbstractDevice [B <: AbstractDeviceInput]\n"

    lazy val obtained = MicroTranslator () .translate_program (original )
    assert (obtained == expected )
  }

  test ("should translate a single lower bound") {
    lazy val original = "  * BlackBox() extends AbstractBlackBox[A superof AbstractInput]\n"

    lazy val expected = "  case class BlackBox () extends AbstractBlackBox [A >: AbstractInput]\n"

    lazy val obtained = MicroTranslator () .translate_program (original )
    assert (obtained == expected )
  }


  test ("should translate multiple lower bounds") {
    lazy val original = "  * BlackBox() extends AbstractBlackBox[A superof AbstractInput] with AbstractDevice[B superof AbstractDeviceInput]\n"

    lazy val expected = "  case class BlackBox () extends AbstractBlackBox [A >: AbstractInput] with AbstractDevice [B >: AbstractDeviceInput]\n"

    lazy val obtained = MicroTranslator () .translate_program (original )
    assert (obtained == expected )
  }

}
