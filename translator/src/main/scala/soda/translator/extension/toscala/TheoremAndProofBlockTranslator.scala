package soda.translator.extension.toscala

trait TheoremAndProofBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ProofBlockAnnotation
  import   soda.translator.parser.annotation.ProofBlockAnnotation_
  import   soda.translator.parser.annotation.TheoremBlockAnnotation
  import   soda.translator.parser.annotation.TheoremBlockAnnotation_

  lazy val tc = TranslationConstantToScala_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case TheoremBlockAnnotation_ (block) => _translate_theorem_block (TheoremBlockAnnotation_ (block) )
      case ProofBlockAnnotation_ (block) => _translate_proof_block (ProofBlockAnnotation_ (block) )
      case x => annotated_block
    }

  def _translate_theorem_block (block : AnnotatedBlock) : TheoremBlockAnnotation =
    TheoremBlockAnnotation_ (_translate_block (block) )

  def _translate_proof_block (block : AnnotatedBlock) : ProofBlockAnnotation =
    ProofBlockAnnotation_ (_translate_block (block) )

  def _translate_block (block : AnnotatedBlock) : Block =
    append (tc.scala_comment_closing_symbol) (prepend (tc.scala_comment_opening_symbol) (block) )

  def prepend (prefix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      Seq [String] (prefix + block.lines.head) ++ block.lines.tail
    )

  def append (suffix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      block.lines.:+ (suffix)
    )

}

case class TheoremAndProofBlockTranslator_ () extends TheoremAndProofBlockTranslator
