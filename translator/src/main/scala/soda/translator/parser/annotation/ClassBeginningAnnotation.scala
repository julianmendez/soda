package soda.translator.parser.annotation

trait ClassBeginningAnnotation  extends BlockAnnotation {

  import soda.translator.parser.SodaConstant_

  lazy val applies: Boolean =
    (starts_with (SodaConstant_ () .class_reserved_word ) ||
      starts_with (SodaConstant_ () .class_abbreviation ) ) &&
    ends_with (SodaConstant_ () .class_open_symbol )

}

case class ClassBeginningAnnotation_ (block: soda.translator.block.Block )  extends ClassBeginningAnnotation
