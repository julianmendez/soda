package soda.translator.parser.annotation

trait ClassDeclarationAnnotation  extends BlockAnnotation {

  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_declaration

  lazy val applies: Boolean =
    (starts_with_prefix_and_space (SodaConstant_ () .class_reserved_word ) ||
      starts_with_prefix_and_space (SodaConstant_ () .class_abbreviation ) ) &&
    ! ends_with_space_and_suffix (SodaConstant_ () .class_open_symbol )

}

case class ClassDeclarationAnnotation_ (block: soda.translator.block.Block )  extends ClassDeclarationAnnotation
