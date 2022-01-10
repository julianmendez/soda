package soda.translator.extension.tocoq

trait CoqProofBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.parser.BlockBuilder_

  lazy val space = " "

  lazy val tc = TranslationConstantToCoq_ ()

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    if (is_a_proof (block )
    ) append (tc.coq_proof_end_reserved_word, prepend (tc.coq_proof_begin_reserved_word, remove_first_line (block ) ) )
    else block

  def prepend (prefix: String, block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (Seq [String] (prefix + block.lines.head ) ++ block.lines.tail, block.block_annotation    )

  def append (suffix: String, block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (block.lines.:+ (suffix ), block.block_annotation    )

  def first_line (block: AnnotatedBlock ): String =
    block.lines.headOption.getOrElse ("") .trim

  def remove_first_line (block: AnnotatedBlock ): AnnotatedBlock =
    if (block.lines.isEmpty
    ) block
    else BlockBuilder_ () .build (block.lines.tail, block.block_annotation )

  def is_a_proof (block: AnnotatedBlock ): Boolean =
    first_line (block ) == tc.proof_reserved_word

}

case class CoqProofBlockTranslator_ ()  extends CoqProofBlockTranslator
