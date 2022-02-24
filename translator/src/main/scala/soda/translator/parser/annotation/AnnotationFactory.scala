package soda.translator.parser.annotation

trait AnnotationFactory
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedBlock_
  import   soda.translator.block.Block
  import   soda.translator.block.BlockAnnotationEnum_

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      if ( block.block_annotation == BlockAnnotationEnum_ ().undefined
      ) annotate (block)
      else block

  def annotate (block : Block) : AnnotatedBlock =
    block match  {
      case AnnotatedBlock_ (annotated_lines, block_annotation) => AnnotatedBlock_ (annotated_lines, block_annotation)
      case x => _get_first_or_undefined (_find_candidates (x) ) (x)
    }

  def update_block (original_content : AnnotatedBlock) (new_content : Block) : AnnotatedBlock =
    original_content match  {
      case FunctionDefinitionAnnotation_ (b) => FunctionDefinitionAnnotation_ (new_content)
      case ClassBeginningAnnotation_ (b) => ClassBeginningAnnotation_ (new_content)
      case ClassEndAnnotation_ (b, references) => ClassEndAnnotation_ (new_content, references)
      case AbstractDeclarationAnnotation_ (b) => AbstractDeclarationAnnotation_ (new_content)
      case ImportDeclarationAnnotation_ (b) => ImportDeclarationAnnotation_ (new_content)
      case PackageDeclarationAnnotation_ (b) => PackageDeclarationAnnotation_ (new_content)
      case ClassAliasAnnotation_ (b) => ClassAliasAnnotation_ (new_content)
      case TheoremBlockAnnotation_ (b) => TheoremBlockAnnotation_ (new_content)
      case ProofBlockAnnotation_ (b) => ProofBlockAnnotation_ (new_content)
      case CommentAnnotation_ (b) => CommentAnnotation_ (new_content)
      case TestDeclarationAnnotation_ (b) => TestDeclarationAnnotation_ (new_content)
      case x => AnnotatedBlock_ (new_content.annotated_lines, x.block_annotation)
    }

  def _detectors (block : Block) : Seq [BlockAnnotationParser] =
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

  def _find_candidates (block : Block) : Seq [BlockAnnotationParser] =
    _detectors (block)
      .filter (  detector => detector.applies)

  def _get_first_or_undefined (candidates : Seq [BlockAnnotationParser] ) (block : Block) : AnnotatedBlock =
    if ( candidates.length == 1
    ) candidates.head
    else AnnotatedBlock_ (block.annotated_lines, BlockAnnotationEnum_ ().undefined )

}

case class AnnotationFactory_ () extends AnnotationFactory
