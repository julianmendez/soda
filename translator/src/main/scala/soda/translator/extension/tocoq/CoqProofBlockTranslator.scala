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

  private lazy val _tc = TranslationConstantToCoq_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ProofBlockAnnotation_ (block) => _translate_block (ProofBlockAnnotation_ (block) )
      case x => annotated_block
    }

  private def _translate_block (block : ProofBlockAnnotation) : ProofBlockAnnotation =
    ProofBlockAnnotation_ (
      _append (
        _tc.coq_proof_end_reserved_word) (
          _replace_first_line (_tc.coq_proof_begin_reserved_word) (block)
      )
    )

  private def _append (suffix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      block.lines.:+ (suffix)
    )

  private def _replace_first_line (first_line : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      Seq (first_line) .++ (_get_tail_or_empty (block.lines) )
    )

  private def _get_tail_or_empty (sequence : Seq [String] ) : Seq [String] =
    if ( sequence.isEmpty
    ) sequence
    else sequence.tail

}

case class CoqProofBlockTranslator_ () extends CoqProofBlockTranslator
