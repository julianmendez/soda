package soda.translator.parser.annotation

trait AnnotationFactory
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedBlock_
  import   soda.translator.block.Block
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.BlockAnnotationId

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      if (block.block_annotation == BlockAnnotationEnum_ () .undefined
      ) annotate (block )
      else block

  def annotate (block: Block ): AnnotatedBlock =
    _get_first_or_undefined (_find_candidates (block ), block )

  def _detectors (block: Block ): Seq [BlockAnnotationParser] =
    Seq (
      FunctionDefinitionAnnotation_ (block ),
      ClassBeginningAnnotation_ (block ),
      ClassEndAnnotation_ (block ),
      AbstractDeclarationAnnotation_ (block ),
      ImportDeclarationAnnotation_ (block ),
      PackageDeclarationAnnotation_ (block ),
      ClassAliasAnnotation_ (block ),
      TheoremBlockAnnotation_ (block ),
      ProofBlockAnnotation_ (block ),
      CommentAnnotation_ (block ),
      TestDeclarationAnnotation_ (block )
    )

  def _find_candidates (block: Block ): Seq [BlockAnnotationParser] =
    _detectors (block )
      .filter (detector => detector.applies )

  def _get_first_or_undefined (candidates: Seq [BlockAnnotationParser], block: Block ): AnnotatedBlock =
    if (candidates.length == 1
    ) candidates.head
    else AnnotatedBlock_ (block.annotated_lines, BlockAnnotationEnum_ () .undefined )

}

case class AnnotationFactory_ ()
  extends
    AnnotationFactory
{

}
