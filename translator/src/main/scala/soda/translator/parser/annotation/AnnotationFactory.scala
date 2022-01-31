package soda.translator.parser.annotation

trait AnnotationFactory
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedBlock_
  import   soda.translator.block.Block
  import   soda.translator.block.BlockAnnotation
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.BlockAnnotationId

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      if (block.block_annotation == BlockAnnotationEnum_ () .undefined
      ) AnnotatedBlock_ (block.annotated_lines, get_annotation (block ) )
      else block

  def detectors (block: Block ): Seq [BlockAnnotation] =
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

  def find_candidates (block: Block ): Seq [BlockAnnotationId] =
    detectors (block )
      .filter (detector => detector.applies )
      .map (detector => detector.identifier )

  def get_annotation (block: Block ): BlockAnnotationId =
    _get_first_or_undefined (find_candidates (block )  )

  def _get_first_or_undefined (candidates: Seq [BlockAnnotationId]  ): BlockAnnotationId =
    if (candidates.length == 1
    ) candidates.head
    else BlockAnnotationEnum_ () .undefined

}

case class AnnotationFactory_ ()
  extends
    AnnotationFactory
{

}
