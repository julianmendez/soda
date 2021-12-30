package soda.translator.parser.annotation

trait ClassDeclarationAnnotation  extends BlockAnnotation {

  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_declaration

  lazy val applies: Boolean =
    contains_one_line &&
    (starts_with (SodaConstant_ () .class_reserved_word ) ||
      starts_with (SodaConstant_ () .class_abbreviation ) ) &&
    ! ends_with (SodaConstant_ () .class_open_symbol )

}

case class ClassDeclarationAnnotation_ (block: soda.translator.block.Block )  extends ClassDeclarationAnnotation
