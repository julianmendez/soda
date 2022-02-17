package soda.translator.extension.toscala

case class BeautifierSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  test ("the translated source code should respect unnecessary spaces")
    {
      lazy val original = "  beautify_this  (  original  : String   )   :  String   =  \n" +
        "    original .  replaceAll(\"  \" ,  \" \")   \n"
      lazy val expected = "  def beautify_this  (  original  : String   )   :  String   =  \n" +
        "    original .  replaceAll(\"  \" ,  \" \")   \n"
      lazy val obtained =
        BlockProcessor_(
          DefaultBlockSequenceTranslator_ (
            MicroTranslatorToScala_()
          )
        ).translate (original)
     assert (obtained == expected) }

}
