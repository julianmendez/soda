package soda.translator.extension.toscala

case class BeautifierSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val original = "  beautify_this  (  original  : String   )   :  String   =  \n" +
    "    original .  replaceAll(\"  \" ,  \" \")   \n"

  test ("the translated source code should respect unnecessary spaces") (
    check (
      obtained =
        BlockProcessor_(
          DefaultBlockSequenceTranslator_ (
            MicroTranslatorToScala_()
          )
        ).translate (original)
    ) (
      expected = "  def beautify_this  (  original  : String   )   :  String   =  \n" +
        "    original .  replaceAll(\"  \" ,  \" \")   \n"
    )
  )

}
