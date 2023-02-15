package soda.translator.parser

/*
 * This package contains common classes used to describe and parse the Soda language.
 */



trait Package

trait BlockBuilder
{

  import   soda.lib.Fold_
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedBlock_
  import   soda.translator.block.Block
  import   soda.translator.block.Block_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _fold = Fold_ ()

  def build (lines : Seq [String] ) : Block =
    Block_ (
      _get_annotated_lines (lines)
    )

  private def _get_annotated_lines (lines : Seq [String] ) : Seq [AnnotatedLine] =
    _fold.apply (lines) (_get_annotated_lines_initial_value) (_get_annotated_lines_next_value_function)
      .annotated_lines_rev
      .reverse

  private lazy val _get_annotated_lines_initial_value  : PreprocessorFoldTuple = PreprocessorFoldTuple_ (false, Seq () )

  private def _get_annotated_lines_next_value_function (pair : PreprocessorFoldTuple) (line : String) : PreprocessorFoldTuple =
    _get_annotated_lines_next_value_function_with (_annotate_this_line (line) (pair.comment_state) ) (pair) (line)

  private def _get_annotated_lines_next_value_function_with (t : CurrentAndNewCommentState) (pair : PreprocessorFoldTuple) (line : String) : PreprocessorFoldTuple =
    PreprocessorFoldTuple_ (t.new_comment_state, pair.annotated_lines_rev.+: (AnnotatedLine_ (line, t.current_state) ) )

  private def _annotate_this_line (line : String) (comment_state : Boolean) : CurrentAndNewCommentState =
    if ( comment_state
    ) CurrentAndNewCommentState_ (true, ! line.trim.endsWith (_sc.comment_closing_symbol) )
    else _annotate_this_line_considering_opening_symbol (line) (comment_state)

  private def _annotate_this_line_considering_opening_symbol (line : String) (comment_state : Boolean) : CurrentAndNewCommentState =
    if ( line.trim.startsWith (_sc.comment_opening_symbol)
    ) CurrentAndNewCommentState_ (true, ! line.trim.endsWith (_sc.comment_closing_symbol) )
    else CurrentAndNewCommentState_ (false, false)

}

case class BlockBuilder_ () extends BlockBuilder

trait PreprocessorFoldTuple
{

  def   comment_state : Boolean
  def   annotated_lines_rev : Seq [soda.translator.block.AnnotatedLine]

}

case class PreprocessorFoldTuple_ (comment_state : Boolean, annotated_lines_rev : Seq [soda.translator.block.AnnotatedLine]) extends PreprocessorFoldTuple

trait CurrentAndNewCommentState
{

  def   current_state : Boolean
  def   new_comment_state : Boolean

}

case class CurrentAndNewCommentState_ (current_state : Boolean, new_comment_state : Boolean) extends CurrentAndNewCommentState


/**
 * An instance of this class splits a String in blocks, applies a translator to them, and joins them again in a String.
 */

trait BlockProcessor
{

  def   translator : soda.translator.block.BlockSequenceTranslator

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.annotation.AnnotationFactory_

  lazy val new_line = "\n"

  lazy val double_new_line = new_line + new_line

  lazy val translator_with_preprocessor = PreprocessorSequenceTranslator_ (translator)

  def translate (program : String) : String =
    join_translated_blocks (
      translator_with_preprocessor.translate (
        split_blocks (program)
      )
    )

  def split_blocks (program : String) : Seq [AnnotatedBlock] =
    program
      .split (double_new_line)
      .toIndexedSeq
      .map (  paragraph => make_block (paragraph) )

  def make_block (paragraph : String) : AnnotatedBlock =
    AnnotationFactory_ ().annotate (
      BlockBuilder_ ().build (
        remove_empty_lines (paragraph.split (new_line).toIndexedSeq)
      )
    )

  def join_translated_blocks (blocks : Seq [AnnotatedBlock] ) : String =
    blocks
      .map (  x => x.contents)
      .mkString (double_new_line) + new_line

  def remove_empty_lines (lines : Seq [String] ) : Seq [String] =
    lines
      .filter (  line => line.trim.nonEmpty)

}

case class BlockProcessor_ (translator : soda.translator.block.BlockSequenceTranslator) extends BlockProcessor


trait PreprocessorSequenceTranslator
  extends
    soda.translator.block.BlockSequenceTranslator
{

  def   translator : soda.translator.block.BlockSequenceTranslator

  import   soda.lib.Fold_
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

  private lazy val _fold = Fold_ ()

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

  private def _get_first_pass (block_sequence : Seq [AnnotatedBlock] ) : Seq [AnnotatedBlock] =
    block_sequence.map (  block => block_annotator.translate (block) )

  private def _get_second_pass (block_sequence : Seq [AnnotatedBlock] ) : Seq [AnnotatedBlock] =
    _fold.apply (block_sequence.indices) (_get_second_pass_initial_value (block_sequence) ) (_get_second_pass_next_value_function)
      .accumulated
      .reverse

  private def _get_second_pass_initial_value (block_sequence : Seq [AnnotatedBlock] ) : AuxiliaryTuple =
    AuxiliaryTuple_ (
      block_sequence = block_sequence,
      accumulated = Seq [AnnotatedBlock] (),
      references = Seq [Seq [AnnotatedBlock] ] ()
    )

  private def _get_second_pass_next_value_function (current : AuxiliaryTuple) (index : Int) : AuxiliaryTuple =
    _pass_next_step (current) (index) (_get_additional_information (current) (index) )

  private def _get_additional_information (current : AuxiliaryTuple) (index : Int) : AnnotatedBlock =
    current.block_sequence.apply (index) match  {
      case AbstractDeclarationAnnotation_ (block, references) => _get_abstract_declaration_updated_block (current) (AbstractDeclarationAnnotation_ (block, references) )
      case ClassEndAnnotation_ (block, references) => _get_class_end_updated_block (current) (ClassEndAnnotation_ (block, references) )
      case x => x
    }

  private def _get_abstract_declaration_updated_block (current : AuxiliaryTuple) (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    AbstractDeclarationAnnotation_ (block.block, block.references.++ (current.references.headOption.getOrElse (Seq [AnnotatedBlock] () ) ) )

  private def _get_class_end_updated_block (current : AuxiliaryTuple) (block : ClassEndAnnotation) : ClassEndAnnotation =
    ClassEndAnnotation_ (block.block, block.references.++ (current.references.headOption.getOrElse (Seq [AnnotatedBlock] () ) ) )

  private def _pass_next_step (current : AuxiliaryTuple) (index : Int) (updated_block : AnnotatedBlock ) : AuxiliaryTuple =
    AuxiliaryTuple_ (
      block_sequence = current.block_sequence,
      accumulated = current.accumulated.+: (updated_block),
      references = _update_references (current) (index)
    )

  private def _update_references (current : AuxiliaryTuple) (index : Int) : Seq [Seq [AnnotatedBlock] ] =
    current.block_sequence.apply (index) match  {
      case ClassBeginningAnnotation_ (b) => current.references.+: (Seq [AnnotatedBlock] (ClassBeginningAnnotation_ (b) ) )
      case AbstractDeclarationAnnotation_ (b, references) => _update_first_element (current.references) (AbstractDeclarationAnnotation_ (b, references) )
      case ClassEndAnnotation_ (b, references) => _tail_non_empty (current.references)
      case x => current.references
    }

  private def _update_first_element (s : Seq [Seq [AnnotatedBlock] ] ) (b : AnnotatedBlock) : Seq [Seq [AnnotatedBlock] ] =
    _tail_non_empty (s).+: (s.headOption.getOrElse (Seq [AnnotatedBlock] () ).+: (b) )

  private def _tail_non_empty [A] (s : Seq [A] ) : Seq [A] =
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


trait SodaConstant
{

  lazy val space = " "

  lazy val new_line = "\n"

  lazy val function_definition_symbol = "="

  lazy val type_membership_symbol = ":"

  lazy val function_arrow_symbol = "->"

  lazy val lambda_arrow_symbol = "-->"

  lazy val case_arrow_symbol = "==>"

  lazy val parameter_definition_symbol = ":="

  lazy val lambda_reserved_word = "lambda"

  lazy val any_reserved_word = "any"

  lazy val if_reserved_word = "if"

  lazy val then_reserved_word = "then"

  lazy val else_reserved_word = "else"

  lazy val match_reserved_word = "match"

  lazy val case_reserved_word = "case"

  lazy val class_reserved_word = "class"

  lazy val extends_reserved_word = "extends"

  lazy val abstract_reserved_word = "abstract"

  lazy val class_end_reserved_word = "end"

  lazy val this_reserved_word = "this"

  lazy val subtype_reserved_word = "subtype"

  lazy val supertype_reserved_word = "supertype"

  lazy val false_reserved_word = "false"

  lazy val true_reserved_word = "true"

  lazy val not_reserved_word = "not"

  lazy val and_reserved_word = "and"

  lazy val or_reserved_word = "or"

  lazy val package_reserved_word = "package"

  lazy val import_reserved_word = "import"

  lazy val theorem_reserved_word = "theorem"

  lazy val proof_reserved_word = "proof"

  lazy val constructor_suffix = "_"

  lazy val test_special_function = "test"

  lazy val subtype_abbreviation = "<:"

  lazy val supertype_abbreviation = ">:"

  lazy val opening_parenthesis_symbol = "("

  lazy val closing_parenthesis_symbol = ")"

  lazy val opening_bracket_symbol = "["

  lazy val closing_bracket_symbol = "]"

  lazy val parameter_separator_symbol = ","

  lazy val addition_symbol = "+"

  lazy val subtraction_symbol = "-"

  lazy val multiplication_symbol = "*"

  lazy val division_symbol = "/"

  lazy val modulo_symbol = "%"

  lazy val documentation_comment_opening_symbol = "/**"

  lazy val comment_opening_symbol = "/*"

  lazy val comment_closing_symbol = "*/"

  lazy val comment_line_symbol = "*"

  lazy val tail_recursion_annotation = "@tailrec"

  lazy val override_annotation = "@override"

  lazy val new_annotation = "@new"

  lazy val private_function_prefix = "_"

  lazy val soda_reserved_words_words_only : Seq [String] =
    Seq (
      lambda_reserved_word,
      any_reserved_word,
      if_reserved_word,
      then_reserved_word,
      else_reserved_word,
      match_reserved_word,
      case_reserved_word,
      class_reserved_word,
      extends_reserved_word,
      abstract_reserved_word,
      class_end_reserved_word,
      this_reserved_word,
      subtype_reserved_word,
      supertype_reserved_word,
      false_reserved_word,
      true_reserved_word,
      not_reserved_word,
      and_reserved_word,
      or_reserved_word,
      package_reserved_word,
      import_reserved_word,
      theorem_reserved_word,
      proof_reserved_word
    )

  lazy val soda_reserved_words_symbols_only : Seq [String] =
    Seq (
      function_definition_symbol,
      type_membership_symbol,
      function_arrow_symbol,
      case_arrow_symbol,
      parameter_definition_symbol,
      addition_symbol,
      subtraction_symbol,
      multiplication_symbol,
      division_symbol,
      modulo_symbol,
      subtype_abbreviation,
      supertype_abbreviation
    )

  lazy val soda_reserved_words_annotations_only : Seq [String] =
    Seq (
      tail_recursion_annotation,
      override_annotation,
      new_annotation
    )

  lazy val soda_reserved_words : Seq [String] =
    soda_reserved_words_words_only.++ (soda_reserved_words_symbols_only.++ (soda_reserved_words_annotations_only) )

}

case class SodaConstant_ () extends SodaConstant

