package soda.translator.parser

trait PreprocessorSequenceTranslator
  extends
    soda.translator.block.BlockSequenceTranslator
{

  def   translator : soda.translator.block.BlockSequenceTranslator

  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.annotation.AnnotationFactory_
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  lazy val block_annotator = AnnotationFactory_ ()

  lazy val ba = soda.translator.block.BlockAnnotationEnum_ ()

  lazy val sc = SodaConstant_ ()

  lazy val recursion = soda.lib.Recursion_ ()

  lazy val empty_line = AnnotatedLine_ ("", true)

  lazy val translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock] =
     block_sequence =>
      translate_for (block_sequence)

  def translate_for (block_sequence : Seq [AnnotatedBlock] ) : Seq [AnnotatedBlock] =
    translator.translate (
      _get_second_pass (
        _get_first_pass (block_sequence)
      )
    )

  def _get_first_pass (block_sequence : Seq [AnnotatedBlock] ) : Seq [AnnotatedBlock] =
    block_sequence.map (  block => block_annotator.translate (block) )

  def _get_second_pass (block_sequence : Seq [AnnotatedBlock] ) : Seq [AnnotatedBlock] =
    recursion
      .fold (block_sequence.indices) (_initial_value (block_sequence) ) (_next_value_function)
      .accumulated
      .reverse

  def _initial_value (block_sequence : Seq [AnnotatedBlock] ) : AuxiliaryTuple =
    AuxiliaryTuple_ (
      block_sequence = block_sequence,
      accumulated = Seq [AnnotatedBlock] (),
      references = Seq [Seq [AnnotatedBlock] ] ()
    )

  def _next_value_function (current : AuxiliaryTuple) (index : Int) : AuxiliaryTuple =
    _pass_next_step (current) (index) (_get_additional_information (current) (index) )

  def _get_additional_information (current : AuxiliaryTuple) (index : Int) : AnnotatedBlock =
    current.block_sequence.apply (index) match  {
      case AbstractDeclarationAnnotation_ (block, references) => _get_abstract_declaration_updated_block (current) (AbstractDeclarationAnnotation_ (block, references) )
      case ClassEndAnnotation_ (block, references) => _get_class_end_updated_block (current) (ClassEndAnnotation_ (block, references) )
      case x => x
    }

  def _get_abstract_declaration_updated_block (current : AuxiliaryTuple) (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    AbstractDeclarationAnnotation_ (block.block, block.references.++ (current.references.headOption.getOrElse (Seq [AnnotatedBlock] () ) ) )

  def _get_class_end_updated_block (current : AuxiliaryTuple) (block : ClassEndAnnotation) : ClassEndAnnotation =
    ClassEndAnnotation_ (block.block, block.references.++ (current.references.headOption.getOrElse (Seq [AnnotatedBlock] () ) ) )

  def _pass_next_step (current : AuxiliaryTuple) (index : Int) (updated_block : AnnotatedBlock ) : AuxiliaryTuple =
    AuxiliaryTuple_ (
      block_sequence = current.block_sequence,
      accumulated = current.accumulated.+: (updated_block),
      references = _update_references (current) (index)
    )

  def _update_references (current : AuxiliaryTuple) (index : Int) : Seq [Seq [AnnotatedBlock] ] =
    current.block_sequence.apply (index) match  {
      case ClassBeginningAnnotation_ (b) => current.references.+: (Seq [AnnotatedBlock] (ClassBeginningAnnotation_ (b) ) )
      case AbstractDeclarationAnnotation_ (b, references) => _update_first_element (current.references) (AbstractDeclarationAnnotation_ (b, references) )
      case ClassEndAnnotation_ (b, references) => _tail_non_empty (current.references)
      case x => current.references
    }

  def _update_first_element (s : Seq [Seq [AnnotatedBlock] ] ) (b : AnnotatedBlock) : Seq [Seq [AnnotatedBlock] ] =
    _tail_non_empty (s).+: (s.headOption.getOrElse (Seq [AnnotatedBlock] () ).+: (b) )

  def _tail_non_empty [A] (s : Seq [A] ) : Seq [A] =
    if ( s.isEmpty
    ) s
    else s.tail

}

case class PreprocessorSequenceTranslator_ (translator : soda.translator.block.BlockSequenceTranslator) extends PreprocessorSequenceTranslator

trait AuxiliaryTuple
{

  def   block_sequence : Seq [soda.translator.block.AnnotatedBlock]
  def   accumulated : Seq [soda.translator.block.AnnotatedBlock]
  def   references : Seq [ Seq [soda.translator.block.AnnotatedBlock] ]

}

case class AuxiliaryTuple_ (block_sequence : Seq [soda.translator.block.AnnotatedBlock], accumulated : Seq [soda.translator.block.AnnotatedBlock], references : Seq [ Seq [soda.translator.block.AnnotatedBlock] ]) extends AuxiliaryTuple
