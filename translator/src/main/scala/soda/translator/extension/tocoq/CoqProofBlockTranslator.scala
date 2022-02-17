package soda.translator.extension.tocoq

trait CoqProofBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ProofBlockAnnotation
  import   soda.translator.parser.annotation.ProofBlockAnnotation_

  lazy val space = " "

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case block : ProofBlockAnnotation => _translate_block (block)
      case x => annotated_block
    }

  def _translate_block (block : ProofBlockAnnotation) : ProofBlockAnnotation =
    ProofBlockAnnotation_ (
      append (
        tc.coq_proof_end_reserved_word) (
          replace_first_line (tc.coq_proof_begin_reserved_word) (block)
      )
    )

  def prepend (prefix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      Seq [String] (prefix + block.lines.head) ++ block.lines.tail
    )

  def append (suffix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      block.lines.:+ (suffix)
    )

  def get_first_line (block : AnnotatedBlock) : String =
    block.lines.headOption.getOrElse ("").trim

  def replace_first_line (first_line : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      Seq (first_line) .++ (get_tail_or_empty (block.lines) )
    )

  def get_tail_or_empty (sequence : Seq [String] ) : Seq [String] =
    if ( sequence.isEmpty
    ) sequence
    else sequence.tail

}

case class CoqProofBlockTranslator_ () extends CoqProofBlockTranslator
