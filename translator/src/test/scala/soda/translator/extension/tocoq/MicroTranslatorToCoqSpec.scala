package soda.translator.extension.tocoq

case class MicroTranslatorToCoqSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.translator.block.BlockProcessor_

  lazy val instance = BlockProcessor_ (MicroTranslatorToCoq_ ()  )

  test ("Coq translation of a constant")
    {
      lazy val original = "x = 0"
      lazy val expected = "Definition\n x := 0\n.\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("Coq translation of a function")
    {
      lazy val original = "f (a: nat) = 0"
      lazy val expected = "Definition\n f (a: nat ) := 0\n.\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

}


