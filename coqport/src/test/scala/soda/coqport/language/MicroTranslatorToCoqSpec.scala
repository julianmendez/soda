package soda.coqport.language


case class MicroTranslatorToCoqSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("Coq translation of a constant")
    {
      lazy val original = "x = 0 ;"
      lazy val expected = "Definition x := 0 .\n"
      lazy val obtained = MicroTranslatorToCoq_ () .translate_program (original )
      assert (obtained == expected ) }

  test ("Coq translation of a function")
    {
      lazy val original = "f (a: nat) = 0 ;"
      lazy val expected = "Definition f (a: nat ) := 0 .\n"
      lazy val obtained = MicroTranslatorToCoq_ () .translate_program (original )
      assert (obtained == expected ) }

}
