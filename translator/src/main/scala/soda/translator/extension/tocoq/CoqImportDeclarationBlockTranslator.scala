package soda.translator.extension.tocoq

/*
 * This package contains classes for the translation to Gallina, the specification language used by Coq.
 */





trait CoqImportDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation_

  private lazy val _tc = TranslationConstantToCoq_ ()

  lazy val coq_import_declaration_pattern =
    _tc.coq_import_reserved_word + _tc.coq_space

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ImportDeclarationAnnotation_ (block) => _translate_block (ImportDeclarationAnnotation_ (block) )
      case x => annotated_block
    }

  private def _translate_block (block : ImportDeclarationAnnotation) : ImportDeclarationAnnotation =
    ImportDeclarationAnnotation_ (
      prepend_to_lines_aligned_at (
        get_number_of_spaces_at_beginning (get_first_line (block) ) ) (
        coq_import_declaration_pattern) (
        block.imported_items
          .filter(  annotated_line => ! annotated_line.is_comment)
          .map (  annotated_line => AnnotatedLine_ (annotated_line.line + _tc.coq_space + _tc.coq_end_symbol, false) )
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

case class CoqImportDeclarationBlockTranslator_ () extends CoqImportDeclarationBlockTranslator
