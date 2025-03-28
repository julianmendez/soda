package soda.translator.parser

/*
 * This package contains common classes used to describe and parse the Soda language.
 */

trait PreprocessorFoldTuple
{

  def   comment_state : Boolean
  def   annotated_lines_rev : Seq [soda.translator.block.AnnotatedLine]

}

case class PreprocessorFoldTuple_ (comment_state : Boolean, annotated_lines_rev : Seq [soda.translator.block.AnnotatedLine]) extends PreprocessorFoldTuple

object PreprocessorFoldTuple {
  def mk (comment_state : Boolean) (annotated_lines_rev : Seq [soda.translator.block.AnnotatedLine]) : PreprocessorFoldTuple =
    PreprocessorFoldTuple_ (comment_state, annotated_lines_rev)
}

trait CurrentAndNewCommentState
{

  def   current_state : Boolean
  def   new_comment_state : Boolean

}

case class CurrentAndNewCommentState_ (current_state : Boolean, new_comment_state : Boolean) extends CurrentAndNewCommentState

object CurrentAndNewCommentState {
  def mk (current_state : Boolean) (new_comment_state : Boolean) : CurrentAndNewCommentState =
    CurrentAndNewCommentState_ (current_state, new_comment_state)
}

trait BlockBuilder
{

  import   soda.lib.Fold
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.Block

  private lazy val _sc = SodaConstant .mk

  private lazy val _fold = Fold .mk

  private def _annotate_this_line_considering_opening_symbol (line : String) (comment_state : Boolean)
      : CurrentAndNewCommentState =
    if ( line .trim .startsWith (_sc .comment_opening_symbol)
    ) CurrentAndNewCommentState .mk (true) (
      ! line .trim .endsWith (_sc .comment_closing_symbol) )
    else CurrentAndNewCommentState .mk (false) (false)

  private def _annotate_this_line (line : String) (comment_state : Boolean) : CurrentAndNewCommentState =
    if ( comment_state
    ) CurrentAndNewCommentState .mk (true) (
      ! line .trim .endsWith (_sc .comment_closing_symbol) )
    else _annotate_this_line_considering_opening_symbol (line) (comment_state)

  private lazy val _get_annotated_lines_initial_value  : PreprocessorFoldTuple =
    PreprocessorFoldTuple .mk (false) (Seq [AnnotatedLine] () )

  private def _get_annotated_lines_next_value_function_with (t : CurrentAndNewCommentState)
      (pair : PreprocessorFoldTuple) (line : String) : PreprocessorFoldTuple =
    PreprocessorFoldTuple .mk (t .new_comment_state) (
      pair .annotated_lines_rev .+: (AnnotatedLine .mk (line) (t .current_state) ) )

  private def _get_annotated_lines_next_value_function (pair : PreprocessorFoldTuple) (line : String)
      : PreprocessorFoldTuple =
    _get_annotated_lines_next_value_function_with (
      _annotate_this_line (line) (pair .comment_state) ) (pair) (line)

  private def _get_annotated_lines (lines : Seq [String] ) : Seq [AnnotatedLine] =
    _fold .apply [String, PreprocessorFoldTuple] (lines) (_get_annotated_lines_initial_value) (
        _get_annotated_lines_next_value_function)
      .annotated_lines_rev
      .reverse

  def build (lines : Seq [String] ) : Block =
    Block .mk (
      _get_annotated_lines (lines)
    )

}

case class BlockBuilder_ () extends BlockBuilder

object BlockBuilder {
  def mk : BlockBuilder =
    BlockBuilder_ ()
}


/**
 * An instance of this class splits a String in blocks, applies a translator to them, and joins them again in a String.
 */

trait BlockProcessor
{

  def   translator : soda.translator.block.BlockSequenceTranslator

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.annotation.AnnotationFactory
  import   soda.translator.replacement.ParserStateEnum
  import   soda.translator.replacement.Token
  import   soda.translator.replacement.Tokenizer
  import   soda.translator.replacement.Replacement

  private lazy val _sc : SodaConstant = SodaConstant .mk

  lazy val fold = soda.lib.Fold .mk

  lazy val new_line = "\n"

  lazy val double_new_line = new_line + new_line

  def split_blocks (program : String) : Seq [String] =
    program
      .split (double_new_line)
      .toIndexedSeq

  def process_unicode_symbols_in_token (token : Token) : String =
    if ( (token .parser_state == ParserStateEnum .mk .plain)
    ) replace_unicode_symbols_in_string (token .text)
    else token .text

  def process_unicode_symbols_in_block (block : String) : String =
    Tokenizer .mk (block)
      .tokens
      .map ( token => process_unicode_symbols_in_token (token) )
      .mkString

  private def _replace_one_unicode_symbol (text : String) (symbol_pair : Tuple2 [String, String] )
      : String =
    Replacement .mk (text)
      .replace_all (symbol_pair ._1) (symbol_pair ._2)
      .line

  def replace_unicode_symbols_in_string (text : String) : String =
    fold [Tuple2 [String, String] , String] (_sc .soda_unicode_symbols) (text) (
      _replace_one_unicode_symbol)

  def remove_empty_lines (lines : Seq [String] ) : Seq [String] =
    lines
      .filter ( line => line .trim .nonEmpty)

  def make_block (paragraph : String) : AnnotatedBlock =
    AnnotationFactory .mk .annotate (
      BlockBuilder .mk .build (
        remove_empty_lines (paragraph .split (new_line) .toIndexedSeq)
      )
    )

  def make_blocks (blocks: Seq [String] ) : Seq[AnnotatedBlock] =
      blocks .map ( paragraph => make_block (paragraph) )

  def join_translated_blocks (blocks : Seq [AnnotatedBlock] ) : String =
    blocks
      .map ( x => x .contents)
      .mkString (double_new_line) + new_line

  lazy val translator_with_preprocessor = PreprocessorSequenceTranslator .mk (translator)

  def translate (program : String) : String =
    join_translated_blocks (
      translator_with_preprocessor .translate (
        split_blocks (program)
          .map ( block => process_unicode_symbols_in_block (block) )
          .map ( block => make_block (block) )
      )
    )

}

case class BlockProcessor_ (translator : soda.translator.block.BlockSequenceTranslator) extends BlockProcessor

object BlockProcessor {
  def mk (translator : soda.translator.block.BlockSequenceTranslator) : BlockProcessor =
    BlockProcessor_ (translator)
}


trait AuxiliaryTuple
{

  def   block_sequence : Seq [soda.translator.block.AnnotatedBlock]
  def   accumulated : Seq [soda.translator.block.AnnotatedBlock]
  def   references : Seq [Seq [soda.translator.block.AnnotatedBlock] ]

}

case class AuxiliaryTuple_ (block_sequence : Seq [soda.translator.block.AnnotatedBlock], accumulated : Seq [soda.translator.block.AnnotatedBlock], references : Seq [Seq [soda.translator.block.AnnotatedBlock] ]) extends AuxiliaryTuple

object AuxiliaryTuple {
  def mk (block_sequence : Seq [soda.translator.block.AnnotatedBlock]) (accumulated : Seq [soda.translator.block.AnnotatedBlock]) (references : Seq [Seq [soda.translator.block.AnnotatedBlock] ]) : AuxiliaryTuple =
    AuxiliaryTuple_ (block_sequence, accumulated, references)
}

trait PreprocessorSequenceTranslator
  extends
    soda.translator.block.BlockSequenceTranslator
{

  def   translator : soda.translator.block.BlockSequenceTranslator

  import   soda.lib.Fold
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.annotation.AnnotationFactory
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  lazy val block_annotator : AnnotationFactory = AnnotationFactory .mk

  lazy val ba = soda.translator.block.BlockAnnotationEnum .mk

  lazy val sc = SodaConstant .mk

  private lazy val _fold = Fold .mk

  lazy val empty_string = ""

  lazy val empty_line : AnnotatedLine = AnnotatedLine .mk (empty_string) (true)

  private def _get_abstract_declaration_updated_block (current : AuxiliaryTuple)
      (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    AbstractDeclarationAnnotation .mk (block .block) (block .references .++ (
      current .references .headOption .getOrElse (Seq [AnnotatedBlock] () ) ) )

  private def _get_class_end_updated_block (current : AuxiliaryTuple) (block : ClassEndAnnotation)
      : ClassEndAnnotation =
    ClassEndAnnotation .mk (block .block) (block .references .++ (
      current .references .headOption .getOrElse (Seq [AnnotatedBlock] () ) ) )

  private def _get_additional_information (current : AuxiliaryTuple) (index : Int) : AnnotatedBlock =
    current .block_sequence .apply (index) match  {
      case AbstractDeclarationAnnotation_ (block, references) =>
        _get_abstract_declaration_updated_block (current) (
          AbstractDeclarationAnnotation .mk (block) (references) )
      case ClassEndAnnotation_ (block, references) =>
        _get_class_end_updated_block (current) (ClassEndAnnotation .mk (block) (references) )
      case _otherwise => current .block_sequence .apply (index)
    }

  private def _get_first_pass (block_sequence : Seq [AnnotatedBlock] ) : Seq [AnnotatedBlock] =
    block_sequence .map ( block => block_annotator .translate (block) )

  private def _get_second_pass_initial_value (block_sequence : Seq [AnnotatedBlock] ) : AuxiliaryTuple =
    AuxiliaryTuple .mk (
      block_sequence = block_sequence ) (
      accumulated = Seq [AnnotatedBlock] () ) (
      references = Seq [Seq [AnnotatedBlock] ] ()
    )

  private def _tail_non_empty [A] (s : Seq [A] ) : Seq [A] =
    if ( s .isEmpty
    ) s
    else s .tail

  private def _update_first_element (s : Seq [Seq [AnnotatedBlock] ] ) (b : AnnotatedBlock)
      : Seq [Seq [AnnotatedBlock] ] =
    _tail_non_empty (s) .+: (s .headOption .getOrElse (Seq [AnnotatedBlock] () ) .+: (b) )

  private def _update_references (current : AuxiliaryTuple) (index : Int) : Seq [Seq [AnnotatedBlock] ] =
    current .block_sequence .apply (index) match  {
      case ClassBeginningAnnotation_ (b) =>
        current .references .+: (Seq [AnnotatedBlock] (ClassBeginningAnnotation .mk (b) ) )
      case AbstractDeclarationAnnotation_ (b, references) =>
        _update_first_element (current .references) (
          AbstractDeclarationAnnotation .mk (b) (references) )
      case ClassEndAnnotation_ (b, references) => _tail_non_empty (current .references)
      case _otherwise => current .references
    }

  private def _pass_next_step (current : AuxiliaryTuple) (index : Int) (updated_block : AnnotatedBlock )
      : AuxiliaryTuple =
    AuxiliaryTuple .mk (
      block_sequence = current .block_sequence) (
      accumulated = current .accumulated .+: (updated_block) ) (
      references = _update_references (current) (index)
    )

  private def _get_second_pass_next_value_function (current : AuxiliaryTuple) (index : Int)
      : AuxiliaryTuple =
    _pass_next_step (current) (index) (_get_additional_information (current) (index) )

  private def _get_second_pass (block_sequence : Seq [AnnotatedBlock] ) : Seq [AnnotatedBlock] =
    _fold .apply [Int, AuxiliaryTuple] (block_sequence .indices) (
      _get_second_pass_initial_value (block_sequence) ) (_get_second_pass_next_value_function)
        .accumulated
        .reverse

  def translate_for (block_sequence : Seq [AnnotatedBlock] ) : Seq [AnnotatedBlock] =
    translator .translate (
      _get_second_pass (
        _get_first_pass (block_sequence)
      )
    )

  lazy val translate : Seq [AnnotatedBlock] => Seq [AnnotatedBlock] =
     block_sequence =>
      translate_for (block_sequence)

}

case class PreprocessorSequenceTranslator_ (translator : soda.translator.block.BlockSequenceTranslator) extends PreprocessorSequenceTranslator

object PreprocessorSequenceTranslator {
  def mk (translator : soda.translator.block.BlockSequenceTranslator) : PreprocessorSequenceTranslator =
    PreprocessorSequenceTranslator_ (translator)
}


trait SodaConstant
{



  lazy val space = " "

  lazy val new_line = "\n"

  lazy val function_definition_symbol = "="

  lazy val function_definition_proposed_unicode_symbol = "\u225D"

  lazy val type_membership_symbol = ":"

  lazy val function_arrow_symbol = "->"

  lazy val function_arrow_unicode_symbol = "\u2192"

  lazy val lambda_arrow_symbol = "-->"

  lazy val lambda_arrow_unicode_symbol = "\u27F6"

  lazy val case_arrow_symbol = "==>"

  lazy val case_arrow_unicode_symbol = "\u27F9"

  lazy val parameter_definition_symbol = ":="

  lazy val parameter_definition_unicode_symbol = "\u2254"

  lazy val any_reserved_word = "any"

  lazy val lambda_reserved_word = "lambda"

  lazy val fun_reserved_word = "fun"

  lazy val lambda_unicode_symbol = "\u03BB"

  lazy val def_reserved_word = "def"

  lazy val if_reserved_word = "if"

  lazy val if_proposed_unicode_symbol = "\u29E9"

  lazy val then_reserved_word = "then"

  lazy val then_proposed_unicode_symbol = "\u25B6"

  lazy val else_reserved_word = "else"

  lazy val else_proposed_unicode_symbol = "\u25B7"

  lazy val match_reserved_word = "match"

  lazy val case_reserved_word = "case"

  lazy val class_reserved_word = "class"

  lazy val class_alias_reserved_word = class_reserved_word

  lazy val class_alias_definition_symbol = "="

  lazy val class_proposed_unicode_symbol = "\u23BE"

  lazy val extends_reserved_word = "extends"

  lazy val abstract_reserved_word = "abstract"

  lazy val abstract_proposed_unicode_symbol = "\u27D0"

  lazy val class_end_reserved_word = "end"

  lazy val class_end_proposed_unicode_symbol = "\u23BF"

  lazy val inductive_reserved_word = "inductive"

  lazy val this_reserved_word = "this"

  lazy val subtype_reserved_word = "subtype"

  lazy val supertype_reserved_word = "supertype"

  lazy val false_reserved_word = "false"

  lazy val false_unicode_symbol = "\u22A5"

  lazy val true_reserved_word = "true"

  lazy val true_unicode_symbol = "\u22A4"

  lazy val not_reserved_word = "not"

  lazy val not_unicode_symbol = "\u00AC"

  lazy val and_reserved_word = "and"

  lazy val and_unicode_symbol = "\u2227"

  lazy val or_reserved_word = "or"

  lazy val or_unicode_symbol = "\u2228"

  lazy val package_reserved_word = "package"

  lazy val import_reserved_word = "import"

  lazy val theorem_reserved_word = "theorem"

  lazy val directive_reserved_word = "directive"

  lazy val constructor_suffix = "_"

  lazy val default_constructor_function = "mk"

  lazy val test_special_function = "test"

  lazy val subtype_abbreviation = "<:"

  lazy val supertype_abbreviation = ">:"

  lazy val opening_parenthesis_symbol = "("

  lazy val closing_parenthesis_symbol = ")"

  lazy val opening_bracket_symbol = "["

  lazy val closing_bracket_symbol = "]"

  lazy val placeholder_symbol = "\u0001"

  lazy val only_blanks_regex = "\\s*"

  lazy val some_blanks_regex = "\\s+"

  lazy val type_parameter_separation_regex = "]" + only_blanks_regex + "\\["

  lazy val class_parameter_separation_regex = "\\)" + only_blanks_regex + "\\("

  lazy val constructor_parameter_separation_regex = "\\(([^)]*)\\)"

  lazy val dot_notation_symbol = "."

  lazy val dot_notation_regex = only_blanks_regex + "\\."

  lazy val type_declaration_colon_regex = ":.*"

  lazy val addition_symbol = "+"

  lazy val subtraction_symbol = "-"

  lazy val multiplication_symbol = "*"

  lazy val division_symbol = "/"

  lazy val modulo_symbol = "%"

  lazy val equals_symbol = "=="

  lazy val less_than_symbol = "<"

  lazy val less_than_or_equal_to_symbol = "<="

  lazy val less_than_or_equal_to_unicode_symbol = "\u2264"

  lazy val greater_than_symbol = ">"

  lazy val greater_than_or_equal_to_symbol = ">="

  lazy val greater_than_or_equal_to_unicode_symbol = "\u2265"

  lazy val empty_list_symbol = "[]"

  lazy val list_constructor_symbol = "::"

  lazy val list_constructor_unicode_symbol = "\u2237"

  lazy val seq_constructor_symbol = "+:"

  lazy val documentation_comment_opening_symbol = "/**"

  lazy val comment_opening_symbol = "/*"

  lazy val comment_closing_symbol = "*/"

  lazy val comment_line_symbol = "*"

  lazy val tail_recursion_annotation = "@tailrec"

  lazy val override_annotation = "@override"

  lazy val new_annotation = "@new"

  lazy val private_function_prefix = "_"

  lazy val intersection_type_symbol = "&"

  lazy val union_type_symbol = "|"

  lazy val main_type_name = "Type"

  lazy val main_type_membership_regex = type_membership_symbol + only_blanks_regex + main_type_name

  lazy val soda_reserved_words_words_only : Seq [String] =
    Seq (
      lambda_reserved_word,
      any_reserved_word,
      def_reserved_word,
      if_reserved_word,
      then_reserved_word,
      else_reserved_word,
      match_reserved_word,
      case_reserved_word,
      class_reserved_word,
      extends_reserved_word,
      abstract_reserved_word,
      class_end_reserved_word,
      inductive_reserved_word,
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
      directive_reserved_word
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
      equals_symbol,
      less_than_symbol,
      less_than_or_equal_to_symbol,
      greater_than_symbol,
      greater_than_or_equal_to_symbol,
      subtype_abbreviation,
      supertype_abbreviation,
      intersection_type_symbol,
      union_type_symbol
    )

  lazy val soda_reserved_words_annotations_only : Seq [String] =
    Seq (
      tail_recursion_annotation,
      override_annotation,
      new_annotation
    )

  lazy val soda_reserved_words : Seq [String] =
    soda_reserved_words_words_only .++ (soda_reserved_words_symbols_only .++ (
      soda_reserved_words_annotations_only) )

  lazy val soda_unicode_symbols : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (lambda_unicode_symbol , lambda_reserved_word) ,
      Tuple2 (lambda_arrow_unicode_symbol , lambda_arrow_symbol) ,
      Tuple2 (function_arrow_unicode_symbol , function_arrow_symbol) ,
      Tuple2 (case_arrow_unicode_symbol , case_arrow_symbol) ,
      Tuple2 (false_unicode_symbol , false_reserved_word) ,
      Tuple2 (true_unicode_symbol , true_reserved_word) ,
      Tuple2 (not_unicode_symbol , not_reserved_word) ,
      Tuple2 (and_unicode_symbol , and_reserved_word) ,
      Tuple2 (or_unicode_symbol , or_reserved_word) ,
      Tuple2 (less_than_or_equal_to_unicode_symbol , less_than_or_equal_to_symbol) ,
      Tuple2 (greater_than_or_equal_to_unicode_symbol , greater_than_or_equal_to_symbol) ,
      Tuple2 (parameter_definition_unicode_symbol , parameter_definition_symbol) ,
      Tuple2 (list_constructor_unicode_symbol , list_constructor_symbol) ,
      Tuple2 (function_definition_proposed_unicode_symbol , function_definition_symbol) ,
      Tuple2 (class_proposed_unicode_symbol , class_reserved_word) ,
      Tuple2 (class_end_proposed_unicode_symbol , class_end_reserved_word) ,
      Tuple2 (abstract_proposed_unicode_symbol , abstract_reserved_word) ,
      Tuple2 (if_proposed_unicode_symbol , if_reserved_word) ,
      Tuple2 (then_proposed_unicode_symbol , then_reserved_word) ,
      Tuple2 (else_proposed_unicode_symbol , else_reserved_word)
    )

}

case class SodaConstant_ () extends SodaConstant

object SodaConstant {
  def mk : SodaConstant =
    SodaConstant_ ()
}

