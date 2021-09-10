package soda.translator.replacement


case class CharTypeSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

   test ("should recognize quotation marks") {
     lazy val input = '"'
     lazy val obtained = CharTypeEnum_ () .get_char_type (input )
     lazy val expected = CharTypeEnum_ () .quotes_type

     assert (obtained == expected )
   }

   test ("should recognize apostrophes") {
     lazy val input = '\''
     lazy val obtained = CharTypeEnum_ () .get_char_type (input )
     lazy val expected = CharTypeEnum_ () .apostrophe_type

     assert (obtained == expected )
   }

   test ("should recognize backslash") {
     lazy val input = '\\'
     lazy val obtained = CharTypeEnum_ () .get_char_type (input )
     lazy val expected = CharTypeEnum_ () .backslash_type

     assert (obtained == expected )
   }

   test ("should recognize a simple char") {
     lazy val input = 'a'
     lazy val obtained = CharTypeEnum_ () .get_char_type (input )
     lazy val expected = CharTypeEnum_ () .plain_type

     assert (obtained == expected )
   }

   test ("should recognize plain text") {
     lazy val inputStr = "This is plain text with symbols. 0123456789 _ . !?"
     lazy val expected = Seq (CharTypeEnum_ () .plain_type )
     lazy val obtained =
       inputStr
         .map (ch => CharTypeEnum_ () .get_char_type (ch )  )
         .toSet
         .toSeq

     assert (obtained == expected )
   }
}
