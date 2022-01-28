package soda.translator.extension.toscala

trait AbstractDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.BlockBuilder_

  lazy val space = " "

  lazy val tc = TranslationConstantToScala_ ()

  lazy val scala_abstract_function_declaration_pattern =
    tc.scala_abstract_function_declaration + space

  lazy val _labels = BlockAnnotationEnum_ ()

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      if (block.block_annotation == _labels.abstract_block_declaration
      ) _translate_block (block )
      else block

  def _translate_block (block: AnnotatedBlock ): AnnotatedBlock =
    if (is_abstract_block_declaration (block )
    ) prepend_to_lines_aligned_at (
      get_number_of_spaces_at_beginning (get_first_line (block ) ),
      scala_abstract_function_declaration_pattern,
      remove_first_line (block ) )
    else block

  def prepend_to_lines_aligned_at (number_of_spaces: Int, prefix: String, block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      block.annotated_lines.map (annotated_line => prepend_aligned_non_comment (number_of_spaces, prefix, annotated_line ) ),
      block.block_annotation
    )

  def prepend_aligned_non_comment (index: Int, prefix: String, annotated_line: AnnotatedLine ): String =
    if (annotated_line.is_comment
    ) annotated_line.line
    else annotated_line.line.substring (0, index ) + prefix + annotated_line.line.substring (index )

  def get_number_of_spaces_at_beginning (line: String ): Int =
    line
      .takeWhile (ch => ch.isSpaceChar )
      .length

  def remove_first_line (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      (if (block.lines.isEmpty
        ) block.lines
        else block.lines.tail ),
      block.block_annotation
    )

  def get_first_line (block: AnnotatedBlock ): String =
    block.lines.headOption.getOrElse ("")

  def is_abstract_block_declaration (block: AnnotatedBlock ): Boolean =
    (get_first_line (block )  ) .trim == tc.abstract_reserved_word

}

case class AbstractDeclarationBlockTranslator_ ()
  extends
    AbstractDeclarationBlockTranslator
{

}
