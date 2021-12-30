package soda.translator.parser.annotation

trait ImportDeclarationAnnotation  extends BlockAnnotation {

  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .import_declaration

  lazy val applies: Boolean =
    block.readable_lines.nonEmpty &&
    block
      .readable_lines
      .forall (annotated_line => is_line_an_import (annotated_line.line ) )

  def is_line_an_import (line: String ): Boolean =
    line.trim.startsWith (SodaConstant_ () .import_reserved_word ) ||
    line.trim.startsWith (SodaConstant_ () .import_abbreviation )

}

case class ImportDeclarationAnnotation_ (block: soda.translator.block.Block )  extends ImportDeclarationAnnotation
