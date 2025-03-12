package soda.translator.replacement

/*
 * This package contains tests for replacement helper functions.
 */

case class CharTypeSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("should recognize quotation marks") (
    check (
      obtained = CharTypeEnum .mk .get_char_type ('"')
    ) (
      expected = CharTypeEnum .mk .quotes_type
    )
  )

  test ("should recognize apostrophes") (
    check (
      obtained = CharTypeEnum .mk .get_char_type ('\'')
    ) (
      expected = CharTypeEnum .mk .apostrophe_type
    )
  )

  test ("should recognize backslash") (
    check (
      obtained = CharTypeEnum .mk .get_char_type ('\\')
    ) (
      expected = CharTypeEnum .mk .backslash_type
    )
  )

  test ("should recognize a simple char") (
    check (
      obtained = CharTypeEnum .mk .get_char_type ('a')
    ) (
      expected = CharTypeEnum .mk .plain_type
    )
  )

  test ("should recognize plain text") (
    check (
      obtained =
        "This is plain text with symbols. 0123456789 _ . !?"
          .map ( ch => CharTypeEnum .mk .get_char_type (ch) )
          .toSet
          .toSeq
    ) (
      expected = Seq (CharTypeEnum .mk .plain_type)
    )
  )

}


case class ReplacementAuxSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance : ReplacementAux = ReplacementAux .mk

  lazy val line_0 = "lambda, if, then, else, match, case"

  lazy val line_1 = "class has extends with this subtype supertype "

  lazy val line_2 = " false ,true ,not ,and ,or ,package ,import ,theorem ,proof ,is ,lambda"

  lazy val line_3 = "  @new, @tailrec, @override "

  lazy val one_word = "lambda"

  test ("replace_if_found_at_beginning 1") (
    check (
      obtained = instance .replace_if_found_at_beginning (line_1) ("class") ("trait")
    ) (
      expected = "trait has extends with this subtype supertype "
    )
  )

  test ("replace_if_found_at_beginning 2") (
    check (
      obtained = instance .replace_if_found_at_beginning (line_1) ("has") ("---")
    ) (
      expected = line_1
    )
  )

  test ("replace_first 1") (
    check (
      obtained = instance .replace_first (line_1) ("class") ("trait")
    ) (
      expected = "trait has extends with this subtype supertype "
    )
  )

  test ("replace_first 2") (
    check (
      obtained = instance .replace_first (line_1) ("this") ("that")
    ) (
      expected = "class has extends with that subtype supertype "
    )
  )

  test ("replace_at 1") (
    check (
      obtained = instance .replace_at (22) (line_1) (" ") ("out ")
    ) (
      expected = "class has extends without this subtype supertype "
    )
  )

  test ("replace_at 2") (
    check (
      obtained = instance .replace_at (-1) (line_1) (" ") ("no replacement here")
    ) (
      expected = line_1
    )
  )

  test ("replace_at 3") (
    check (
      obtained = instance .replace_at (line_1 .length - 1) (line_1) (" ") ("")
    ) (
      expected = "class has extends with this subtype supertype"
    )
  )

  test ("replace_at 4") (
    check (
      obtained = instance .replace_at (line_1 .length) (line_1) (" ") ("no replacement")
    ) (
      expected = line_1
    )
  )

  test ("replace_if_found") (
    check (
      obtained = instance .replace_if_found (line_1) ("type") ("class")
    ) (
      expected = "class has extends with this subclass superclass "
    )
  )

  test ("replace_all 1") (
    check (
      obtained = instance .replace_all (line_1) (" ") (",")
    ) (
      expected = "class,has,extends,with,this,subtype,supertype,"
    )
  )

  test ("replace_all 2") (
    check (
      obtained = instance .replace_all (line_1) ("z") ("-")
    ) (
      expected = line_1
    )
  )

  test ("add_spaces_to_symbols 1") (
    check (
      obtained = instance .add_spaces_to_symbols (line_0) ( (Seq [Char] (',') ) .toSet)
    ) (
      expected = "lambda , if , then , else , match , case"
    )
  )

  test ("add_spaces_to_symbols 2") (
    check (
      obtained = instance .add_spaces_to_symbols (line_2) ( (Seq [Char] (',') ) .toSet)
    ) (
      expected =
        " false , true , not , and , or , package , import , theorem , proof , is , lambda"
    )
  )

  test ("add_spaces_to_symbols 3") (
    check (
      obtained = instance .add_spaces_to_symbols (line_1) ( (Seq [Char] (
        'a', 'e', 'i', 'o', 'u') ) .toSet)
    ) (
      expected = "cl a ss h a s e xt e nds w i th th i s s u btyp e s u p e rtyp e "
    )
  )

  test ("remove_space_from_scala_line") (
    check (
      obtained = instance .remove_space_from_translated_line (line_3)
    ) (
      expected = " @new, @tailrec, @override"
    )
  )

  test ("add_after_spaces_or_pattern 1") (
    check (
      obtained = instance .add_after_spaces_or_pattern (line_1) ("class") (" here")
    ) (
      expected = "class here has extends with this subtype supertype "
    )
  )

  test ("add_after_spaces_or_pattern 2") (
    check (
      obtained = instance .add_after_spaces_or_pattern (line_1) ("has") (" here ")
    ) (
      expected = " here class has extends with this subtype supertype "
    )
  )

  test ("add_after_spaces_or_pattern 3") (
    check (
      obtained = instance .add_after_spaces_or_pattern (line_3) ("@new") (", here")
    ) (
      expected = "  @new, here, @tailrec, @override "
    )
  )

}


case class ReplacementSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.parser.SodaConstant

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance : ReplacementAux = ReplacementAux .mk

  private lazy val _sc : SodaConstant = SodaConstant .mk

  test ("Unicode replacements 1") (
    check (
      obtained =
        Replacement .mk ("\u03BB")
          .replace_all ("\u03BB") (_sc .lambda_reserved_word)
          .line
    ) (
      expected = "lambda"
    )
  )

  test ("Unicode replacement 2") (
    check (
      obtained =
        Replacement .mk ("\u2192")
          .replace_all ("\u2192") ("->")
          .line
    ) (
      expected = "->"
    )
  )

  test ("Unicode replacement 3") (
    check (
      obtained =
        Replacement .mk (_sc .case_arrow_unicode_symbol)
          .replace_all (_sc .case_arrow_unicode_symbol) (_sc .case_reserved_word)
          .line
    ) (
      expected = _sc .case_reserved_word
    )
  )

}


case class TokenizerSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("should tokenize a small example") (
    check (
      obtained = Tokenizer .mk ("    val Constant = \"my text\"") .tokens
    ) (
      expected = Seq (
        Token .mk ("    val Constant = ") (ParserStateEnum .mk .plain) (0),
        Token .mk ("\"my text\"") (ParserStateEnum .mk .quotes_state) (19),
        Token .mk ("") (ParserStateEnum .mk .plain) (28)
      )
    )
  )

  test ("should tokenize a common tab in a string") (
    check (
      obtained = Tokenizer .mk ("  x = \"abc\tde\"") .tokens
    ) (
      expected = Seq (
        Token .mk ("  x = ") (ParserStateEnum .mk .plain) (0),
        Token .mk ("\"abc\tde\"") (ParserStateEnum .mk .quotes_state) (6),
        Token .mk ("") (ParserStateEnum .mk .plain) (14)
      )
    )
  )

  test ("should tokenize an escaped tab in a string") (
    check (
      obtained = Tokenizer .mk ("  x = \"abc\\tde\"") .tokens
    ) (
      expected = Seq (
        Token .mk ("  x = ") (ParserStateEnum .mk .plain) (0),
        Token .mk ("\"abc\\tde\"") (ParserStateEnum .mk .quotes_state) (6),
        Token .mk ("") (ParserStateEnum .mk .plain) (15)
      )
    )
  )

  test ("should tokenize a single function definition") (
    check (
      obtained = Tokenizer .mk ("def f (x: Int): Int = x") .tokens
    ) (
      expected = Seq (
        Token .mk ("def f (x: Int): Int = x") (ParserStateEnum .mk .plain) (0)
      )
    )
  )

  test ("should tokenize a function call") (
    check (
      obtained = Tokenizer .mk ("\tas_digits (5 * number)") .tokens
    ) (
      expected = Seq (
        Token .mk ("\tas_digits (5 * number)") (ParserStateEnum .mk .plain) (0)
      )
    )
  )

}

