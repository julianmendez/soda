package soda.translator.extension.toscala

trait ClassEndBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  lazy val tc = TranslationConstantToScala_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassEndAnnotation_ (block, references) => _translate_block (ClassEndAnnotation_ (block, references) )
      case x => annotated_block
    }

  def _translate_block (block : ClassEndAnnotation) : ClassEndAnnotation =
    ClassEndAnnotation_ (
      BlockBuilder_ ().build (
        Seq [String] (
          _get_translation (block)
        )
      ),
      block.references
    )

  def _get_translation (block : ClassEndAnnotation) : String =
    _get_initial_spaces (block) + tc.scala_class_end_symbol

  def _get_initial_spaces (block : ClassEndAnnotation) : String =
    _get_initial_spaces_with (_get_first_line (block) )

  def _get_initial_spaces_with (line : String) : String =
    line.takeWhile (  ch => ch.isSpaceChar)

  def _get_first_line (block : AnnotatedBlock) : String =
    block.lines.headOption.getOrElse ("")

}

case class ClassEndBlockTranslator_ () extends ClassEndBlockTranslator
