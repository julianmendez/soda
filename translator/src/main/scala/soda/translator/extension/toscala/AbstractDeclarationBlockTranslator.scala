package soda.translator.extension.toscala

trait AbstractDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.Block
  import   soda.translator.block.Block_
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation_

  lazy val space = " "

  lazy val tc = TranslationConstantToScala_ ()

  lazy val scala_abstract_function_declaration_pattern =
    tc.scala_abstract_function_declaration + space

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case AbstractDeclarationAnnotation_ (block) => _translate_block (AbstractDeclarationAnnotation_ (block) )
      case x => annotated_block
    }

  def _translate_block (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    AbstractDeclarationAnnotation_ (
      prepend_to_lines_aligned_at (
        get_number_of_spaces_at_beginning (get_first_line (block) ) ) (
        scala_abstract_function_declaration_pattern) (
        block.abstract_functions_with_comments
      )
    )

  def prepend_to_lines_aligned_at (number_of_spaces : Int) (prefix : String) (annotated_lines : Seq [AnnotatedLine] ) : Block =
    Block_ (
      annotated_lines.map (  annotated_line => prepend_aligned_non_comment (number_of_spaces) (prefix) (annotated_line) )
    )

  def prepend_aligned_non_comment (index : Int) (prefix : String) (annotated_line : AnnotatedLine) : AnnotatedLine =
    if ( annotated_line.is_comment
    ) annotated_line
    else AnnotatedLine_ (annotated_line.line.substring (0, index) + prefix + annotated_line.line.substring (index), annotated_line.is_comment)

  def get_number_of_spaces_at_beginning (line : String) : Int =
    line
      .takeWhile (ch => ch.isSpaceChar)
      .length

  def get_first_line (block : AnnotatedBlock) : String =
    block.lines.headOption.getOrElse ("")

}

case class AbstractDeclarationBlockTranslator_ () extends AbstractDeclarationBlockTranslator
