package soda.coqport.language


trait MicroTranslatorSpec  extends org.scalatest.funsuite.AnyFunSuite {

  test ("Coq translation") {
    lazy val original = "x = 0"
    lazy val expected = "x = 0"
    /* FIXME */

    lazy val obtained = MicroTranslator_ () .translate_program (original )

    assert (obtained == expected )
  }
}

case class MicroTranslatorSpec_ () extends MicroTranslatorSpec
