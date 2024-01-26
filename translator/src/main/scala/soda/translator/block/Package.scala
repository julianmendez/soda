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

object AnnotatedBlock {
  def mk (annotated_lines : Seq [AnnotatedLine]) (block_annotation : BlockAnnotationId) : AnnotatedBlock =
    AnnotatedBlock_ (annotated_lines, block_annotation)
}


trait AnnotatedLine
{

  def   line : String
  def   is_comment : Boolean

}

case class AnnotatedLine_ (line : String, is_comment : Boolean) extends AnnotatedLine

object AnnotatedLine {
  def mk (line : String) (is_comment : Boolean) : AnnotatedLine =
    AnnotatedLine_ (line, is_comment)
}

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

object Block {
  def mk (annotated_lines : Seq [AnnotatedLine]) : Block =
    Block_ (annotated_lines)
}


trait BlockAnnotationId
  extends
    soda.lib.EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class BlockAnnotationId_ (ordinal : Int, name : String) extends BlockAnnotationId

object BlockAnnotationId {
  def mk (ordinal : Int) (name : String) : BlockAnnotationId =
    BlockAnnotationId_ (ordinal, name)
}

trait BlockAnnotationEnum
  extends
    soda.lib.Enum [BlockAnnotationId]
{

  private def _mk_BlockAnnotationId (ordinal : Int) (name : String) : BlockAnnotationId =
    BlockAnnotationId_ (ordinal, name)

  lazy val undefined = _mk_BlockAnnotationId (0) ("undefined")

  lazy val function_definition = _mk_BlockAnnotationId (1) ("function_definition")

  lazy val class_beginning = _mk_BlockAnnotationId (2) ("class_beginning")

  lazy val class_end = _mk_BlockAnnotationId (3) ("class_end")

  lazy val abstract_declaration = _mk_BlockAnnotationId (4) ("abstract_declaration")

  lazy val import_declaration = _mk_BlockAnnotationId (5) ("import_declaration")

  lazy val package_declaration = _mk_BlockAnnotationId (6) ("package_declaration")

  lazy val class_alias = _mk_BlockAnnotationId (7) ("class_alias")

  lazy val theorem_block = _mk_BlockAnnotationId (8) ("theorem_block")

  lazy val directive_block = _mk_BlockAnnotationId (9) ("directive_block")

  lazy val comment = _mk_BlockAnnotationId (10) ("comment")

  lazy val test_declaration = _mk_BlockAnnotationId (11) ("test_declaration")

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

object BlockAnnotationEnum {
  def mk : BlockAnnotationEnum =
    BlockAnnotationEnum_ ()
}


trait BlockSequenceTranslator
{

  def   translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock]

}

case class BlockSequenceTranslator_ (translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock]) extends BlockSequenceTranslator

object BlockSequenceTranslator {
  def mk (translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock]) : BlockSequenceTranslator =
    BlockSequenceTranslator_ (translate)
}

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

object DefaultBlockSequenceTranslator {
  def mk (translator : BlockTranslator) : DefaultBlockSequenceTranslator =
    DefaultBlockSequenceTranslator_ (translator)
}


trait BlockTranslator
{

  def   translate : AnnotatedBlock => AnnotatedBlock

}

case class BlockTranslator_ (translate : AnnotatedBlock => AnnotatedBlock) extends BlockTranslator

object BlockTranslator {
  def mk (translate : AnnotatedBlock => AnnotatedBlock) : BlockTranslator =
    BlockTranslator_ (translate)
}

trait DefaultBlockTranslator
  extends
    BlockTranslator
{

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      block

}

case class DefaultBlockTranslator_ () extends DefaultBlockTranslator

object DefaultBlockTranslator {
  def mk : DefaultBlockTranslator =
    DefaultBlockTranslator_ ()
}


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

object BlockTranslatorPipeline {
  def mk (pipeline : Seq [BlockTranslator]) : BlockTranslatorPipeline =
    BlockTranslatorPipeline_ (pipeline)
}


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

object ConditionalBlockTranslator {
  def mk (accepted_annotations : Seq [BlockAnnotationId]) (translator : BlockTranslator) : ConditionalBlockTranslator =
    ConditionalBlockTranslator_ (accepted_annotations, translator)
}


trait SingleLineProcessor
{

  def   line : String

}

case class SingleLineProcessor_ (line : String) extends SingleLineProcessor

object SingleLineProcessor {
  def mk (line : String) : SingleLineProcessor =
    SingleLineProcessor_ (line)
}


trait LineTranslator
{

  def   line : String

}

case class LineTranslator_ (line : String) extends LineTranslator

object LineTranslator {
  def mk (line : String) : LineTranslator =
    LineTranslator_ (line)
}


trait PlainBlock
{

  def   lines : Seq [String]

  lazy val new_line = "\n"

  lazy val contents : String =
    lines .mkString (new_line)

}

case class PlainBlock_ (lines : Seq [String]) extends PlainBlock

object PlainBlock {
  def mk (lines : Seq [String]) : PlainBlock =
    PlainBlock_ (lines)
}


/**
 * This models an abstract translator.
 */

trait Translator
{

  def   translate : String => String
  def   keys : Seq [String]

}

case class Translator_ (translate : String => String, keys : Seq [String]) extends Translator

object Translator {
  def mk (translate : String => String) (keys : Seq [String]) : Translator =
    Translator_ (translate, keys)
}

