package scopus.translator

import org.scalatest.funsuite.AnyFunSuite


case class CharTypeSpec() extends AnyFunSuite {


   test("should recognize quotation marks") {
     lazy val input = '"'
     lazy val obtained = CharTypeEnum().get_char_type(input)
     lazy val expected = CharTypeEnum().QuotesType
     assert(obtained == expected)
   }

   test("should recognize apostrophes") {
     lazy val input = '\''
     lazy val obtained = CharTypeEnum().get_char_type(input)
     lazy val expected = CharTypeEnum().ApostropheType
     assert(obtained == expected)
   }

   test("should recognize backslash") {
     lazy val input = '\\'
     lazy val obtained = CharTypeEnum().get_char_type(input)
     lazy val expected = CharTypeEnum().BackslashType
     assert(obtained == expected)
   }

   test("should recognize a simple char") {
     lazy val input = 'a'
     lazy val obtained = CharTypeEnum().get_char_type(input)
     lazy val expected = CharTypeEnum().PlainType
     assert(obtained == expected)
   }

   test("should recognize plain text") {
     lazy val inputStr = "This is plain text with symbols. 0123456789 _ . !?"
     lazy val expected = Seq( CharTypeEnum().PlainType )
     lazy val obtained =
       inputStr
         .map(ch => CharTypeEnum().get_char_type(ch))
         .toSet
         .toSeq
     assert(obtained == expected)
   }

}
