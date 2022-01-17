package soda.translator.extension.tocoq

trait CoqDefinitionBlockTranslator
  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.BlockBuilder_

  lazy val space = " "

  lazy val tc = TranslationConstantToCoq_ ()

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    if (block.block_annotation == BlockAnnotationEnum_ () .function_definition
    ) _translate_block (block )
    else block

  def _translate_block (block: AnnotatedBlock ): AnnotatedBlock =
    if (is_a_recursive_definition (block )
    ) append (tc.coq_recursive_definition_end, prepend (tc.coq_recursive_definition + space, block ) )
    else
      if (is_a_definition (block )
      ) append (tc.coq_definition_end, prepend (tc.coq_definition + space, block ) )
      else block

  def prepend (prefix: String, block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      Seq [String] (prefix + block.lines.head ) ++ block.lines.tail, block.block_annotation
    )

  def append (suffix: String, block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      block.lines.:+ (suffix ), block.block_annotation
    )

  def is_a_recursive_definition (block: AnnotatedBlock ): Boolean =
    tc.coq_recursive_function_prefixes.exists (prefix => first_line (block ) .startsWith (prefix )  )

  def first_line (block: AnnotatedBlock ): String =
    block.lines.headOption.getOrElse ("") .trim

  def is_a_definition (block: AnnotatedBlock ): Boolean =
    ! is_a_recursive_definition (block ) &&
    ! tc.non_definition_block_prefixes.exists (prefix => block.contents.trim.startsWith (prefix )  )

}

case class CoqDefinitionBlockTranslator_ ()
  extends CoqDefinitionBlockTranslator
