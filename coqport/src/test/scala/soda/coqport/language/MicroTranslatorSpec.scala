package soda.coqport.language


case class MicroTranslatorSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("Coq translation") {
    lazy val original = "x = 0"
    lazy val expected = "x = 0\n"
    /* FIXME */

    lazy val obtained = MicroTranslator_ () .translate_program (original )

    assert (obtained == expected )
  }
}
