package soda.translator.parser.annotation

trait PackageDeclarationAnnotation  extends BlockAnnotation {

  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .package_declaration

  lazy val applies: Boolean =
    contains_one_line &&
    starts_with (SodaConstant_ () .package_reserved_word + SodaConstant_ () .space )

}

case class PackageDeclarationAnnotation_ (block: soda.translator.block.Block )  extends PackageDeclarationAnnotation
