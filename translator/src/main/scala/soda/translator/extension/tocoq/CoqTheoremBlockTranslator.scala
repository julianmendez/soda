package soda.translator.extension.tocoq

trait CoqTheoremBlockTranslator
  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.BlockBuilder_

  lazy val space = " "

  lazy val tc = TranslationConstantToCoq_ ()

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    if (block.block_annotation == BlockAnnotationEnum_ () .theorem_block
    ) _translate_block (block )
    else block

  def _translate_block (block: AnnotatedBlock ): AnnotatedBlock =
    if (is_a_theorem (block )
    ) append (
      tc.coq_theorem_end, prepend (
        tc.coq_theorem_begin_reserved_word, remove_first_line (block ) ) )
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

  def remove_first_line (block: AnnotatedBlock ): AnnotatedBlock =
    if (block.lines.isEmpty
    ) block
    else BlockBuilder_ () .build (block.lines.tail, block.block_annotation )

  def is_a_theorem (block: AnnotatedBlock ): Boolean =
    first_line (block ) == tc.theorem_reserved_word

}

case class CoqTheoremBlockTranslator_ ()
  extends CoqTheoremBlockTranslator
