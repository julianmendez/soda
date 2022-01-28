package soda.translator.parser

trait BlockAnnotator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedBlock_
  import   soda.translator.block.Block
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.BlockAnnotationId
  import   soda.translator.parser.annotation.BlockAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassEndAnnotation_
  import   soda.translator.parser.annotation.ClassDeclarationAnnotation_
  import   soda.translator.parser.annotation.AbstractFunctionDeclarationAnnotation_
  import   soda.translator.parser.annotation.AbstractBlockDeclarationAnnotation_
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation_
  import   soda.translator.parser.annotation.PackageDeclarationAnnotation_
  import   soda.translator.parser.annotation.ProofBlockAnnotation_
  import   soda.translator.parser.annotation.CommentAnnotation_
  import   soda.translator.parser.annotation.TestDeclarationAnnotation_
  import   soda.translator.parser.annotation.TheoremBlockAnnotation_

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      if (block.block_annotation == BlockAnnotationEnum_ () .undefined
      ) AnnotatedBlock_ (block.lines, block.annotated_lines, get_annotation (block ) )
      else block

  def detectors (block: Block ): Seq [BlockAnnotation] =
    Seq (
      FunctionDefinitionAnnotation_ (block ),
      ClassBeginningAnnotation_ (block ),
      ClassEndAnnotation_ (block ),
      ClassDeclarationAnnotation_ (block ),
      AbstractBlockDeclarationAnnotation_ (block ),
      AbstractFunctionDeclarationAnnotation_ (block ),
      ImportDeclarationAnnotation_ (block ),
      PackageDeclarationAnnotation_ (block ),
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

case class BlockAnnotator_ ()
  extends
    BlockAnnotator
{

}
