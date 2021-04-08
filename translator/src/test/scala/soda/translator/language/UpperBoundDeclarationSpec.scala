package soda.translator.language

import org.scalatest.funsuite.AnyFunSuite


case class UpperBoundDeclarationSpec (  ) extends AnyFunSuite {

  test ("should translate a single upper bound") {
    lazy val original = "  * BlackBox() extends AbstractBlackBox[A extends AbstractInput]\n"

    lazy val expected = "  case class BlackBox (  ) extends AbstractBlackBox [ A <: AbstractInput ]\n"

    lazy val obtained = MicroTranslator (  ) .translate_program ( original )
    assert ( obtained == expected )
  }


  test ("should translate multiple upper bounds") {
    lazy val original = "  * BlackBox() extends AbstractBlackBox[A extends AbstractInput] with AbstractDevice[B extends AbstractDeviceInput]\n"

    lazy val expected = "  case class BlackBox (  ) extends AbstractBlackBox [ A <: AbstractInput ] with AbstractDevice [ B <: AbstractDeviceInput ]\n"

    lazy val obtained = MicroTranslator (  ) .translate_program ( original )
    assert ( obtained == expected )
  }

}
