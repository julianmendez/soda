package soda.translator.extension.toscala

trait MainClassBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  lazy val sc = SodaConstant_ ()

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
    if (_get_class_name (block.references ) == tc.soda_main_class_name
    )
      ClassEndAnnotation_ (
        BlockBuilder_ () .build (
          Seq [String] (
            tc.scala_class_end_symbol,
            "",
            tc.scala_entry_point
          )
        ),
        block.references
      )
    else block

  def _get_class_name (references: Seq [AnnotatedBlock] ): String =
    _get_class_beginning (references )
      .map (x => x.class_name )
      .getOrElse ("")

  def _get_class_beginning (references: Seq [AnnotatedBlock] ): Option [ClassBeginningAnnotation] =
    references
      .flatMap (block =>
        block match  {
          case b: ClassBeginningAnnotation => Some (b )
          case x => None
        }
      )
      .headOption

}

case class MainClassBlockTranslator_ () extends MainClassBlockTranslator
