package soda.translator.extension.toscala

trait ImportDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation_

  lazy val space = " "

  lazy val tc = TranslationConstantToScala_ ()

  lazy val scala_import_declaration_pattern =
    tc.scala_import_declaration + space

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case block : ImportDeclarationAnnotation => _translate_block (block)
      case x => annotated_block
    }

  def _translate_block (block : ImportDeclarationAnnotation) : ImportDeclarationAnnotation =
    ImportDeclarationAnnotation_ (
      prepend_to_lines_aligned_at (
        get_number_of_spaces_at_beginning (get_first_line (block) ) ) (
        scala_import_declaration_pattern) (
        block.imported_items
      )
    )

  def prepend_to_lines_aligned_at (number_of_spaces : Int) (prefix : String) (annotated_lines : Seq [AnnotatedLine] ) : Block =
    BlockBuilder_ ().build (
      annotated_lines.map (  annotated_line => prepend_aligned_non_comment (number_of_spaces) (prefix) (annotated_line) )
    )

  def prepend_aligned_non_comment (index : Int) (prefix : String) (annotated_line : AnnotatedLine) : String =
    if ( annotated_line.is_comment
    ) annotated_line.line
    else annotated_line.line.substring (0, index) + prefix + annotated_line.line.substring (index)

  def get_number_of_spaces_at_beginning (line : String) : Int =
    line
      .takeWhile (  ch => ch.isSpaceChar)
      .length

  def get_first_line (block : AnnotatedBlock) : String =
    block.lines.headOption.getOrElse ("")

}

case class ImportDeclarationBlockTranslator_ () extends ImportDeclarationBlockTranslator
