package soda.translator.parser.annotation

trait CommentAnnotation  extends BlockAnnotation {

  import soda.translator.parser.SodaConstant_

  lazy val applies: Boolean =
    block
      .annotated_lines
      .forall (annotated_line => annotated_line.isComment )

}

case class CommentAnnotation_ (block: soda.translator.block.Block )  extends CommentAnnotation
