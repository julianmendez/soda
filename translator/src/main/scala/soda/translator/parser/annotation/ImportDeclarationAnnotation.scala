package soda.translator.parser.annotation

trait ImportDeclarationAnnotation
  extends BlockAnnotation {

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .import_declaration

  lazy val applies: Boolean =
    block.readable_lines.nonEmpty &&
    (block.readable_lines.head.line.trim == SodaConstant_ () .import_reserved_word )

}

case class ImportDeclarationAnnotation_ (block: soda.translator.block.Block )
  extends ImportDeclarationAnnotation
