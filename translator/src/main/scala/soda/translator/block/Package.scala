package soda.translator.block

/*
 * This package contains abstract and concrete classes to define a block and a block translator.
 */



trait Package

trait AnnotatedBlock
  extends
    Block
{

  def   annotated_lines : Seq [AnnotatedLine]
  def   block_annotation : BlockAnnotationId

}

case class AnnotatedBlock_ (annotated_lines : Seq [AnnotatedLine], block_annotation : BlockAnnotationId) extends AnnotatedBlock


trait AnnotatedLine
{

  def   line : String
  def   is_comment : Boolean

}

case class AnnotatedLine_ (line : String, is_comment : Boolean) extends AnnotatedLine

trait Block
  extends
    PlainBlock
{

  def   annotated_lines : Seq [AnnotatedLine]

  lazy val lines : Seq [String] =
    annotated_lines
      .map ( x => x .line)

  lazy val readable_lines : Seq [AnnotatedLine] =
    annotated_lines
      .filter ( line => ! line .is_comment)

}

case class Block_ (annotated_lines : Seq [AnnotatedLine]) extends Block


trait BlockAnnotationId
  extends
    soda.lib.EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class BlockAnnotationId_ (ordinal : Int, name : String) extends BlockAnnotationId

trait BlockAnnotationEnum
  extends
    soda.lib.Enum [BlockAnnotationId]
{

  lazy val undefined = BlockAnnotationId_ (0 , "undefined")

  lazy val function_definition = BlockAnnotationId_ (1 , "function_definition")

  lazy val class_beginning = BlockAnnotationId_ (2 , "class_beginning")

  lazy val class_end = BlockAnnotationId_ (3 , "class_end")

  lazy val abstract_declaration = BlockAnnotationId_ (4 , "abstract_declaration")

  lazy val import_declaration = BlockAnnotationId_ (5 , "import_declaration")

  lazy val package_declaration = BlockAnnotationId_ (6 , "package_declaration")

  lazy val class_alias = BlockAnnotationId_ (7 , "class_alias")

  lazy val theorem_block = BlockAnnotationId_ (8 , "theorem_block")

  lazy val directive_block = BlockAnnotationId_ (9 , "directive_block")

  lazy val comment = BlockAnnotationId_ (10 , "comment")

  lazy val test_declaration = BlockAnnotationId_ (11 , "test_declaration")

  lazy val values =
    Seq (
      undefined,
      function_definition,
      class_beginning,
      class_end,
      abstract_declaration,
      import_declaration,
      package_declaration,
      class_alias,
      theorem_block,
      directive_block,
      comment,
      test_declaration
    )

}

case class BlockAnnotationEnum_ () extends BlockAnnotationEnum


trait BlockSequenceTranslator
{

  def   translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock]

}

case class BlockSequenceTranslator_ (translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock]) extends BlockSequenceTranslator

trait DefaultBlockSequenceTranslator
  extends
    BlockSequenceTranslator
{

  def   translator : BlockTranslator

  lazy val translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock] =
     block_sequence =>
      block_sequence .map ( block => translator .translate (block) )

}

case class DefaultBlockSequenceTranslator_ (translator : BlockTranslator) extends DefaultBlockSequenceTranslator


trait BlockTranslator
{

  def   translate : AnnotatedBlock => AnnotatedBlock

}

case class BlockTranslator_ (translate : AnnotatedBlock => AnnotatedBlock) extends BlockTranslator

trait DefaultBlockTranslator
  extends
    BlockTranslator
{

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      block

}

case class DefaultBlockTranslator_ () extends DefaultBlockTranslator


trait BlockTranslatorPipeline
  extends
    BlockTranslator
{

  import   soda.lib.Fold_

  def   pipeline : Seq [BlockTranslator]

  private lazy val _fold = Fold_ ()

  private def _next_value (block : AnnotatedBlock) (translator : BlockTranslator) : AnnotatedBlock =
    translator .translate (block)

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _fold .apply (pipeline) (block) (_next_value)

}

case class BlockTranslatorPipeline_ (pipeline : Seq [BlockTranslator]) extends BlockTranslatorPipeline


trait ConditionalBlockTranslator
  extends
    BlockTranslator
{

  def   accepted_annotations : Seq [BlockAnnotationId]
  def   translator : BlockTranslator

  def translate_for (block : AnnotatedBlock) : AnnotatedBlock =
    if ( accepted_annotations.contains (block .block_annotation)
    ) translator .translate (block)
    else block

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class ConditionalBlockTranslator_ (accepted_annotations : Seq [BlockAnnotationId], translator : BlockTranslator) extends ConditionalBlockTranslator


trait SingleLineProcessor
{

  def   line : String

}

case class SingleLineProcessor_ (line : String) extends SingleLineProcessor


trait LineTranslator
{

  def   line : String

}

case class LineTranslator_ (line : String) extends LineTranslator


trait PlainBlock
{

  def   lines : Seq [String]

  lazy val new_line = "\n"

  lazy val contents : String =
    lines .mkString (new_line)

}

case class PlainBlock_ (lines : Seq [String]) extends PlainBlock


/**
 * This models an abstract translator.
 */

trait Translator
{

  def   translate : String => String
  def   keys : Seq [String]

}

case class Translator_ (translate : String => String, keys : Seq [String]) extends Translator

