package soda.translator.extension.toscala

/*
 * This package contains classes for the translation to Scala.
 */





trait MainClassBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  private lazy val _tc = TranslationConstantToScala_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassEndAnnotation_ (block, references) => _translate_block (ClassEndAnnotation_ (block, references) )
      case x => annotated_block
    }

  private def _translate_block (block : ClassEndAnnotation) : ClassEndAnnotation =
    if ( _get_class_name (block.references) == _tc.soda_main_class_name
    )
      ClassEndAnnotation_ (
        BlockBuilder_ ().build (
          Seq [String] (
            _tc.scala_class_end_symbol,
            "",
            _tc.scala_entry_point
          )
        ),
        block.references
      )
    else block

  private def _get_class_name (references : Seq [AnnotatedBlock] ) : String =
    _get_class_beginning (references)
      .map (  x => x.class_name)
      .getOrElse ("")

  private def _get_class_beginning (references : Seq [AnnotatedBlock] ) : Option [ClassBeginningAnnotation] =
    references
      .flatMap (  block => _get_as_class_beginning_annotation (block) )
      .headOption

  private def _get_as_class_beginning_annotation (block : AnnotatedBlock) : Option [ClassBeginningAnnotation] =
    block match  {
      case ClassBeginningAnnotation_ (b) => Some (ClassBeginningAnnotation_ (b) )
      case x => None
    }

}

case class MainClassBlockTranslator_ () extends MainClassBlockTranslator
