package soda.translator.parser

/*
 * This package contains tests for the Soda parser.
 */

case class BlockSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.AnnotatedLine_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  private def _mk_AnnotatedLine (line : String) (is_comment : Boolean) : AnnotatedLine_ =
    AnnotatedLine_ (line, is_comment)

  lazy val input = ("\n" +
    "\n/** This is an example */" +
    "\n* Example () {" +
    "\n  /* This is a comment */" +
    "\n  a = \"/** this is not a comment */\"" +
    "\n}" +
    "\n")
    .split ("\n")
    .toSeq

  /* This is to test how to find commented text. */
  test ("should find commented text") (
    check (
      obtained = BlockBuilder_ () .build (input) .annotated_lines
    ) (
      expected = Seq (
        _mk_AnnotatedLine ("") (is_comment = false) ,
        _mk_AnnotatedLine ("") (is_comment = false) ,
        _mk_AnnotatedLine ("/** This is an example */") (is_comment = true) ,
        _mk_AnnotatedLine ("* Example () {") (is_comment = false) ,
        _mk_AnnotatedLine ("  /* This is a comment */") (is_comment = true) ,
        _mk_AnnotatedLine ("  a = \"/** this is not a comment */\"") (is_comment = false) ,
        _mk_AnnotatedLine ("}") (is_comment = false)
      )
    )
  )

}


case class PreprocessorSequenceTranslatorSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.block.DefaultBlockTranslator_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_program =
    ("package soda.example.mytest" +
    "\n" +
    "\n/**" +
    "\n * Example class for testing." +
    "\n * '\u03BB' in a comment is not replaced." +
    "\n */" +
    "\n" +
    "\n\u23BE Example" +
    "\n" +
    "\n  import" +
    "\n    soda.lib.Fold_" +
    "\n    soda.lib.Enum" +
    "\n" +
    "\n  \u27D0" +
    "\n    number : Int" +
    "\n    name : String" +
    "\n" +
    "\n  my_constant : Int = 0" +
    "\n" +
    "\n  my_function (x : Int) (y : Int) : Int \u225D" +
    "\n    x + y" +
    "\n" +
    "\n  another_function (f : Int \u2192 Int) (x : Int) : Int =" +
    "\n    f (x)" +
    "\n" +
    "\n  process (sequence : Seq [Int] ) : Seq [Int] =" +
    "\n    sequence" +
    "\n      .map (\u03BB elem \u27F6 my_function (x \u2254 elem) (y \u2254 my_constant) )" +
    "\n      .map ( \u03BB elem \u27F6 my_function (x \u2254 elem) (y \u2254 my_constant) )" +
    "\n" +
    "\n  my_xor (x : Boolean) (y : Boolean) : Boolean =" +
    "\n    (x \u2228 y) \u2227 \u00AC (x \u2227 y)"  +
    "\n" +
    "\n  are_equal (x : Int) (y : Int) : Boolean =" +
    "\n    (x \u2264 y) \u2227 (x \u2265 y)"  +
    "\n" +
    "\n  if_true_else (b : Boolean) (x : Int) (y : Int) : Int =" +
    "\n    match b" +
    "\n      case \u22A4 \u27F9 x"  +
    "\n      case \u22A5 \u27F9 y"  +
    "\n" +
    "\n  my_min (x : Int) (y : Int) : Int =" +
    "\n    \u29E9 x < y" +
    "\n     \u25B6 x"  +
    "\n     \u25B7 y"  +
    "\n" +
    "\n  length [A : Type] (list : Seq [A] ) : Int =" +
    "\n    match list" +
    "\n      case Nil ==> 0"  +
    "\n      case (head) \u2237 (tail) ==> length [A] (tail) + 1" +
    "\n" +
    "\n\u23BF" +
    "\n" +
    "\n")

  lazy val expected_output =
    ("package soda.example.mytest" +
    "\n" +
    "\n/**" +
    "\n * Example class for testing." +
    "\n * '\u03BB' in a comment is not replaced." +
    "\n */" +
    "\n" +
    "\nclass Example" +
    "\n" +
    "\n  import" +
    "\n    soda.lib.Fold_" +
    "\n    soda.lib.Enum" +
    "\n" +
    "\n  abstract" +
    "\n    number : Int" +
    "\n    name : String" +
    "\n" +
    "\n  my_constant : Int = 0" +
    "\n" +
    "\n  my_function (x : Int) (y : Int) : Int =" +
    "\n    x + y" +
    "\n" +
    "\n  another_function (f : Int -> Int) (x : Int) : Int =" +
    "\n    f (x)" +
    "\n" +
    "\n  process (sequence : Seq [Int] ) : Seq [Int] =" +
    "\n    sequence" +
    "\n      .map (lambda elem --> my_function (x := elem) (y := my_constant) )" +
    "\n      .map ( lambda elem --> my_function (x := elem) (y := my_constant) )" +
    "\n" +
    "\n  my_xor (x : Boolean) (y : Boolean) : Boolean =" +
    "\n    (x or y) and not (x and y)"  +
    "\n" +
    "\n  are_equal (x : Int) (y : Int) : Boolean =" +
    "\n    (x <= y) and (x >= y)"  +
    "\n" +
    "\n  if_true_else (b : Boolean) (x : Int) (y : Int) : Int =" +
    "\n    match b" +
    "\n      case true ==> x"  +
    "\n      case false ==> y"  +
    "\n" +
    "\n  my_min (x : Int) (y : Int) : Int =" +
    "\n    if x < y" +
    "\n     then x"  +
    "\n     else y"  +
    "\n" +
    "\n  length [A : Type] (list : Seq [A] ) : Int =" +
    "\n    match list" +
    "\n      case Nil ==> 0"  +
    "\n      case (head) :: (tail) ==> length [A] (tail) + 1" +
    "\n" +
    "\nend" +
    "\n")

  lazy val block_processor =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        DefaultBlockTranslator_ ()
      )
    )

  test ("should test the preprocessor") (
    check (
      obtained = block_processor .translate (example_program)
    ) (
      expected = expected_output
    )
  )

}

