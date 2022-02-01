package soda.translator.extension.toscala

trait ClassEndBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  lazy val tc = TranslationConstantToScala_ ()

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block )

  def translate_for (annotated_block: AnnotatedBlock ): AnnotatedBlock =
    annotated_block match  {
      case block: ClassEndAnnotation => _translate_block (block )
      case x => annotated_block
    }

  def _translate_block (block: ClassEndAnnotation ): ClassEndAnnotation =
    ClassEndAnnotation_ (
      BlockBuilder_ () .build (Seq [String] (tc.scala_class_end_symbol ) )
    )

}

case class ClassEndBlockTranslator_ ()
  extends
    ClassEndBlockTranslator
{

}
