package soda.translator.extension.toscala

trait TheoremAndProofBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ProofBlockAnnotation
  import   soda.translator.parser.annotation.TheoremBlockAnnotation

  lazy val tc = TranslationConstantToScala_ ()

  lazy val _labels = BlockAnnotationEnum_ ()

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block )

  def translate_for (annotated_block: AnnotatedBlock ): AnnotatedBlock =
    annotated_block match  {
      case block: TheoremBlockAnnotation => _translate_block (block )
      case block: ProofBlockAnnotation => _translate_block (block )
      case x => annotated_block
    }

  def _translate_block (block: AnnotatedBlock ): AnnotatedBlock =
    if (is_a_theorem_or_a_proof (block )
    ) append (
      tc.comment_close, prepend (
        tc.comment_open, block ) )
    else block

  def prepend (prefix: String, block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      Seq [String] (prefix + block.lines.head ) ++ block.lines.tail, block.block_annotation
    )

  def append (suffix: String, block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      block.lines.:+ (suffix ), block.block_annotation
    )

  def first_line (block: AnnotatedBlock ): String =
    block.lines.headOption.getOrElse ("") .trim

  def is_a_theorem_or_a_proof (block: AnnotatedBlock ): Boolean =
    _is_line_a_theorem_or_a_proof (first_line (block ) )

  def _is_line_a_theorem_or_a_proof (line: String ): Boolean =
    line == tc.theorem_reserved_word ||
    line == tc.proof_reserved_word

}

case class TheoremAndProofBlockTranslator_ ()
  extends
    TheoremAndProofBlockTranslator
{

}
