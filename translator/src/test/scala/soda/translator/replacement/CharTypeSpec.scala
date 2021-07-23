package soda.translator.replacement


case class CharTypeSpec () extends org.scalatest.funsuite.AnyFunSuite {

   test ("should recognize quotation marks") {
     lazy val input = '"'
     lazy val obtained = CharTypeEnumImpl () .get_char_type (input )
     lazy val expected = CharTypeEnumImpl () .QuotesType

     assert (obtained == expected )
   }

   test ("should recognize apostrophes") {
     lazy val input = '\''
     lazy val obtained = CharTypeEnumImpl () .get_char_type (input )
     lazy val expected = CharTypeEnumImpl () .ApostropheType

     assert (obtained == expected )
   }

   test ("should recognize backslash") {
     lazy val input = '\\'
     lazy val obtained = CharTypeEnumImpl () .get_char_type (input )
     lazy val expected = CharTypeEnumImpl () .BackslashType

     assert (obtained == expected )
   }

   test ("should recognize a simple char") {
     lazy val input = 'a'
     lazy val obtained = CharTypeEnumImpl () .get_char_type (input )
     lazy val expected = CharTypeEnumImpl () .PlainType

     assert (obtained == expected )
   }

   test ("should recognize plain text") {
     lazy val inputStr = "This is plain text with symbols. 0123456789 _ . !?"
     lazy val expected = Seq (CharTypeEnumImpl () .PlainType )
     lazy val obtained =
       inputStr
         .map (ch => CharTypeEnumImpl () .get_char_type (ch )  )
         .toSet
         .toSeq

     assert (obtained == expected )
   }
}
