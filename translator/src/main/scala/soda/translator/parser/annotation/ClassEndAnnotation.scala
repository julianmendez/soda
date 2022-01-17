package soda.translator.parser.annotation

trait ClassEndAnnotation
  extends BlockAnnotation {

  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_end

  lazy val applies: Boolean =
    block.readable_lines.nonEmpty &&
    (block.readable_lines.head.line.trim == SodaConstant_ () .class_close_symbol )

}

case class ClassEndAnnotation_ (block: soda.translator.block.Block )
  extends ClassEndAnnotation
