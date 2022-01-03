package soda.translator.parser.annotation

case class BlockAnnotationSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.translator.block.Block
  import soda.translator.block.DefaultBlockTranslator_
  import soda.translator.parser.BlockProcessor_

  lazy val example_program =
    ("package soda.example.mytest" +
    "\n" +
    "\n/**" +
    "\n * Example class for testing." +
    "\n */" +
    "\n" +
    "\nclass Example = {" +
    "\n" +
    "\n  import soda.lib.Recursion_" +
    "\n  + soda.lib.Enum" +
    "\n" +
    "\n  has text: String" +
    "\n" +
    "\n  my_constant: Int = 0" +
    "\n" +
    "\n  my_function (x: Int, y: Int): Int =" +
    "\n    x + y" +
    "\n" +
    "\n  test (\"should test the example\")" +
    "\n    let" +
    "\n      result = true" +
    "\n    in assert (result)" +
    "\n" +
    "\n}" +
    "\n" +
    "\n* Example_ () extends Example" +
    "\n" +
    "\n")

  lazy val example_blocks: Seq [Block] =
    BlockProcessor_ (DefaultBlockTranslator_ ()  ) .split_blocks (example_program )

  def detectors (block: Block ): Seq [BlockAnnotation] =
    Seq (FunctionDefinitionAnnotation_ (block ), ClassBeginningAnnotation_ (block ), ClassEndAnnotation_ (block ), ClassDeclarationAnnotation_ (block ), AbstractFunctionDeclarationAnnotation_ (block ), ImportDeclarationAnnotation_ (block ), PackageDeclarationAnnotation_ (block ), CommentAnnotation_ (block ), TestDeclarationAnnotation_ (block )    )

  def apply_detectors (block: Block ): Seq [Boolean] =
    detectors (block ) .map (detector => detector.applies )

  test ("should detect a package declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, false, false, true, false, false )
      lazy val obtained = apply_detectors (example_blocks (0 )  )
      assert (obtained == expected ) }

  test ("should detect a comment")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, false, false, false, true, false )
      lazy val obtained = apply_detectors (example_blocks (1 )  )
      assert (obtained == expected ) }

  test ("should detect a class beginning")
    {
      lazy val expected: Seq [Boolean] = Seq (false, true, false, false, false, false, false, false, false )
      lazy val obtained = apply_detectors (example_blocks (2 )  )
      assert (obtained == expected ) }

  test ("should detect an import declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, false, true, false, false, false )
      lazy val obtained = apply_detectors (example_blocks (3 )  )
      assert (obtained == expected ) }

  test ("should detect an abstract function declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, true, false, false, false, false )
      lazy val obtained = apply_detectors (example_blocks (4 )  )
      assert (obtained == expected ) }

  test ("should detect a constant declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (true, false, false, false, false, false, false, false, false )
      lazy val obtained = apply_detectors (example_blocks (5 )  )
      assert (obtained == expected ) }

  test ("should detect a function declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (true, false, false, false, false, false, false, false, false )
      lazy val obtained = apply_detectors (example_blocks (6 )  )
      assert (obtained == expected ) }

  test ("should detect a test declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, false, false, false, false, true )
      lazy val obtained = apply_detectors (example_blocks (7 )  )
      assert (obtained == expected ) }

  test ("should detect a class end")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, true, false, false, false, false, false, false )
      lazy val obtained = apply_detectors (example_blocks (8 )  )
      assert (obtained == expected ) }

  test ("should detect a class declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, true, false, false, false, false, false )
      lazy val obtained = apply_detectors (example_blocks (9 )  )
      assert (obtained == expected ) }

  test ("should find only 10 blocks")
    {
      lazy val expected = 10
      lazy val obtained = example_blocks.length
      assert (obtained == expected ) }

  test ("should be ordered by the identifier ordinal")
    {
      lazy val expected: Seq [Int] = Seq (1, 2, 3, 4, 5, 6, 7, 8, 9 )
      lazy val obtained = detectors (example_blocks (0 )  ) .map (detector => detector.identifier.ordinal )
      assert (obtained == expected ) }

}
