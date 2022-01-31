package soda.translator.extension.toscala

trait ClassEndBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ClassEndAnnotation

  lazy val tc = TranslationConstantToScala_ ()

  lazy val _labels = BlockAnnotationEnum_ ()

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block )

  def translate_for (annotated_block: AnnotatedBlock ): AnnotatedBlock =
    annotated_block match  {
      case block: ClassEndAnnotation => _translate_block (block )
      case x => annotated_block
    }

  def _translate_block (block: ClassEndAnnotation ): AnnotatedBlock =
    if (is_class_end (block )
    ) BlockBuilder_ () .build (Seq [String] (tc.scala_class_end_symbol ), block.block_annotation )
    else block

  def get_first_line_trimmed (block: AnnotatedBlock ): String =
    block.lines.headOption.getOrElse ("") .trim

  def is_class_end (block: AnnotatedBlock ): Boolean =
    _is_class_end_with (get_first_line_trimmed (block ) )

  def _is_class_end_with (line: String ): Boolean =
    line == tc.class_end_reserved_word ||
    line == tc.class_end_symbol

}

case class ClassEndBlockTranslator_ ()
  extends
    ClassEndBlockTranslator
{

}
