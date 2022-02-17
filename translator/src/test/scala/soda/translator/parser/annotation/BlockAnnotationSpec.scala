package soda.translator.parser.annotation

case class BlockAnnotationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.Block
  import   soda.translator.block.DefaultBlockTranslator_
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  lazy val example_blocks =
    ExampleProgram_ ().example_blocks

  def detectors (block: Block): Seq [BlockAnnotationParser] =
    Seq (
      FunctionDefinitionAnnotation_ (block),
      ClassBeginningAnnotation_ (block),
      ClassEndAnnotation_ (block, Seq [BlockAnnotationParser] () ),
      AbstractDeclarationAnnotation_ (block),
      ImportDeclarationAnnotation_ (block),
      PackageDeclarationAnnotation_ (block),
      ClassAliasAnnotation_ (block),
      TheoremBlockAnnotation_ (block),
      ProofBlockAnnotation_ (block),
      CommentAnnotation_ (block),
      TestDeclarationAnnotation_ (block)
    )

  def apply_detectors (block: Block): Seq [Boolean] =
    detectors (block).map (  detector => detector.applies)

  test ("should detect a package declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, false, true, false, false, false, false, false)
      lazy val obtained = apply_detectors (example_blocks (0) )
     assert (obtained == expected) }

  test ("should detect a comment")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, false, false, false, false, false, true, false)
      lazy val obtained = apply_detectors (example_blocks (1) )
     assert (obtained == expected) }

  test ("should detect a class beginning")
    {
      lazy val expected: Seq [Boolean] = Seq (false, true, false, false, false, false, false, false, false, false, false)
      lazy val obtained = apply_detectors (example_blocks (2) )
     assert (obtained == expected) }

  test ("should detect an abstract block declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, true, false, false, false, false, false, false, false)
      lazy val obtained = apply_detectors (example_blocks (3) )
     assert (obtained == expected) }

  test ("should detect an import declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, true, false, false, false, false, false, false)
      lazy val obtained = apply_detectors (example_blocks (4) )
     assert (obtained == expected) }

  test ("should detect a constant declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (true, false, false, false, false, false, false, false, false, false, false)
      lazy val obtained = apply_detectors (example_blocks (5) )
     assert (obtained == expected) }

  test ("should detect a function declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (true, false, false, false, false, false, false, false, false, false, false)
      lazy val obtained = apply_detectors (example_blocks (6) )
     assert (obtained == expected) }

  test ("should detect a test declaration")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, false, false, false, false, false, false, true)
      lazy val obtained = apply_detectors (example_blocks (7) )
     assert (obtained == expected) }

  test ("should detect a theorem block")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, false, false, false, true, false, false, false)
      lazy val obtained = apply_detectors (example_blocks (8) )
     assert (obtained == expected) }

  test ("should detect a proof block")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, false, false, false, false, true, false, false)
      lazy val obtained = apply_detectors (example_blocks (9) )
     assert (obtained == expected) }

  test ("should detect a class end")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, true, false, false, false, false, false, false, false, false)
      lazy val obtained = apply_detectors (example_blocks (10) )
     assert (obtained == expected) }

  test ("should detect a class alias")
    {
      lazy val expected: Seq [Boolean] = Seq (false, false, false, false, false, false, true, false, false, false, false)
      lazy val obtained = apply_detectors (example_blocks (11) )
     assert (obtained == expected) }

  test ("should find only 12 blocks")
    {
      lazy val expected = 12
      lazy val obtained = example_blocks.length
     assert (obtained == expected) }

  test ("should be ordered by the identifier ordinal")
    {
      lazy val expected: Seq [Int] = Seq (1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
      lazy val obtained = detectors (example_blocks (0) ).map (  detector => detector.identifier.ordinal)
     assert (obtained == expected) }

}
