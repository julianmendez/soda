package soda.translator.extension.tocoq

trait CoqTheoremBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.TheoremBlockAnnotation
  import   soda.translator.parser.annotation.TheoremBlockAnnotation_

  private lazy val _tc = TranslationConstantToCoq_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case TheoremBlockAnnotation_ (block) => _translate_block (TheoremBlockAnnotation_ (block) )
      case x => annotated_block
    }

  private def _translate_block (block : TheoremBlockAnnotation) : TheoremBlockAnnotation =
    TheoremBlockAnnotation_ (
      _append (
        _tc.coq_theorem_end_symbol) (_prepend (
          _tc.coq_theorem_begin_reserved_word) (_remove_first_line (block)
        )
      )
    )

  private def _prepend (prefix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      Seq[String] (prefix + block.lines.head).++ (block.lines.tail)
    )

  private def _append (suffix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      block.lines.:+ (suffix)
    )

  private def _remove_first_line (block : Block) : Block =
    if ( block.lines.isEmpty
    ) block
    else BlockBuilder_ ().build (block.lines.tail)

}

case class CoqTheoremBlockTranslator_ () extends CoqTheoremBlockTranslator
