package soda.translator.extension.tocoq

trait CoqDefinitionBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.parser.BlockBuilder_

  lazy val space = " "

  lazy val tc = TranslationConstantToCoq_ ()

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    if (is_a_recursive_definition (block ) ) append (tc.coq_recursive_definition_end, prepend (tc.coq_recursive_definition + space, block ) )
    else if (is_a_definition (block ) ) append (tc.coq_definition_end, prepend (tc.coq_definition + space, block ) )
    else block

  def prepend (prefix: String, block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (Seq [String] (prefix + block.lines.head ) ++ block.lines.tail    )

  def append (suffix: String, block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (block.lines.:+ (suffix )    )

  def is_a_recursive_definition (block: AnnotatedBlock ): Boolean =
    {
      lazy val first = block.lines.head.trim
      lazy val result = tc.coq_recursive_function_prefixes.exists (prefix => first.startsWith (prefix )  )
      result }

  def is_a_definition (block: AnnotatedBlock ): Boolean =
    {
      lazy val contents = block.contents.trim
      lazy val result =
        ! is_a_recursive_definition (block ) &&
        ! tc.non_definition_block_prefixes.exists (prefix => contents.startsWith (prefix )  )
      result }

}

case class CoqDefinitionBlockTranslator_ ()  extends CoqDefinitionBlockTranslator
