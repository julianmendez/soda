package soda.translator.parser.annotation

trait PackageDeclarationAnnotation  extends BlockAnnotation {

  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .package_declaration

  lazy val applies: Boolean =
    starts_with_prefix_and_space (SodaConstant_ () .package_reserved_word )

}

case class PackageDeclarationAnnotation_ (block: soda.translator.block.Block )  extends PackageDeclarationAnnotation
