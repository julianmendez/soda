package soda.translator.parser.annotation

/*
 * This package contains classes to handle block annotations for parsing.
 */

trait Package

trait AbstractDeclarationAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block
  def   references : Seq [soda.translator.block.AnnotatedBlock]

  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .abstract_declaration

  lazy val applies : Boolean =
    block .readable_lines .nonEmpty &&
    (block .readable_lines .head .line .trim == SodaConstant_ () .abstract_reserved_word)

  lazy val abstract_functions_with_comments : Seq [AnnotatedLine] =
    content_lines

  lazy val abstract_functions : Seq [AnnotatedLine] =
    abstract_functions_with_comments
      .filter ( line => ! line .is_comment)

}

case class AbstractDeclarationAnnotation_ (block : soda.translator.block.Block, references : Seq [soda.translator.block.AnnotatedBlock]) extends AbstractDeclarationAnnotation


trait AnnotationFactory
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedBlock_
  import   soda.translator.block.Block
  import   soda.translator.block.BlockAnnotationEnum_

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (block : AnnotatedBlock) : AnnotatedBlock =
    if ( block .block_annotation == BlockAnnotationEnum_ () .undefined
    ) annotate (block)
    else block

  def annotate (block : Block) : AnnotatedBlock =
    block match  {
      case AnnotatedBlock_ (annotated_lines, block_annotation) => AnnotatedBlock_ (annotated_lines , block_annotation)
      case otherwise => _get_first_or_undefined (_find_candidates (block) ) (block)
    }

  def update_block (original_content : AnnotatedBlock) (new_content : Block) : AnnotatedBlock =
    original_content match  {
      case FunctionDefinitionAnnotation_ (b) => FunctionDefinitionAnnotation_ (new_content)
      case ClassBeginningAnnotation_ (b) => ClassBeginningAnnotation_ (new_content)
      case ClassEndAnnotation_ (b , references) => ClassEndAnnotation_ (new_content , references)
      case AbstractDeclarationAnnotation_ (b , references) => AbstractDeclarationAnnotation_ (new_content , references)
      case ImportDeclarationAnnotation_ (b) => ImportDeclarationAnnotation_ (new_content)
      case PackageDeclarationAnnotation_ (b) => PackageDeclarationAnnotation_ (new_content)
      case ClassAliasAnnotation_ (b) => ClassAliasAnnotation_ (new_content)
      case TheoremBlockAnnotation_ (b) => TheoremBlockAnnotation_ (new_content)
      case ProofBlockAnnotation_ (b) => ProofBlockAnnotation_ (new_content)
      case CommentAnnotation_ (b) => CommentAnnotation_ (new_content)
      case TestDeclarationAnnotation_ (b) => TestDeclarationAnnotation_ (new_content)
      case otherwise => AnnotatedBlock_ (new_content .annotated_lines , original_content .block_annotation)
    }

  private def _detectors (block : Block) : Seq [BlockAnnotationParser] =
    Seq (
      FunctionDefinitionAnnotation_ (block),
      ClassBeginningAnnotation_ (block),
      ClassEndAnnotation_ (block , Seq [BlockAnnotationParser] () ),
      AbstractDeclarationAnnotation_ (block , Seq [BlockAnnotationParser] () ),
      ImportDeclarationAnnotation_ (block),
      PackageDeclarationAnnotation_ (block),
      ClassAliasAnnotation_ (block),
      TheoremBlockAnnotation_ (block),
      ProofBlockAnnotation_ (block),
      CommentAnnotation_ (block),
      TestDeclarationAnnotation_ (block)
    )

  private def _find_candidates (block : Block) : Seq [BlockAnnotationParser] =
    _detectors (block)
      .filter ( detector => detector .applies)

  private def _get_first_or_undefined (candidates : Seq [BlockAnnotationParser] ) (block : Block) : AnnotatedBlock =
    if ( candidates .length == 1
    ) candidates .head
    else AnnotatedBlock_ (block .annotated_lines , BlockAnnotationEnum_ () .undefined )

}

case class AnnotationFactory_ () extends AnnotationFactory


trait BlockAnnotationParser
  extends
    soda.translator.block.AnnotatedBlock
{

  def   block : soda.translator.block.Block
  def   applies : Boolean
  def   identifier : soda.translator.block.BlockAnnotationId

  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.Block
  import   soda.translator.block.BlockAnnotationId
  import   soda.translator.parser.SodaConstant_

  lazy val space = SodaConstant_ () .space

  lazy val default_annotated_line = AnnotatedLine_ ("", true)

  lazy val annotated_lines : Seq [AnnotatedLine] = block .annotated_lines

  lazy val block_annotation : BlockAnnotationId = identifier

  def starts_with_prefix_and_space (prefix : String) : Boolean =
    block .readable_lines .nonEmpty &&
    block .readable_lines .head .line .trim .startsWith (prefix + space)

  lazy val content_lines : Seq [AnnotatedLine] =
    if ( block .readable_lines .isEmpty
    ) block .annotated_lines
    else
      block
        .annotated_lines
        .tail
        .filter ( x => ! x .line .trim .isEmpty)

  lazy val first_readable_line : AnnotatedLine =
    block .readable_lines .headOption .getOrElse (default_annotated_line)

  def get_first_word (line : String) : String =
    (_get_first_word_with (line .trim .indexOf (space) ) (line) ) .trim

  private def _get_first_word_with (index : Int) (line : String) : String =
    if ( index >= 0
    ) line .substring (0, index)
    else line

  def skip_first_word (line : String) : String =
    (_skip_first_word_with (line .trim .indexOf (space) ) (line) ) .trim

  private def _skip_first_word_with (index : Int) (line : String) : String =
    if ( index >= 0
    ) line .trim .substring (index)
    else ""

}

case class BlockAnnotationParser_ (block : soda.translator.block.Block, applies : Boolean, identifier : soda.translator.block.BlockAnnotationId) extends BlockAnnotationParser


trait ClassAliasAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_alias

  lazy val sc = SodaConstant_ ()

  lazy val applies : Boolean =
     starts_with_prefix_and_space (sc .class_reserved_word) &&
     _contains_the_equals_symbol

  private lazy val _contains_the_equals_symbol : Boolean =
    FunctionDefinitionAnnotation_ (block) .contains_the_equals_symbol

}

case class ClassAliasAnnotation_ (block : soda.translator.block.Block) extends ClassAliasAnnotation


trait ClassBeginningAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_beginning

  lazy val sc = SodaConstant_ ()

  lazy val applies : Boolean =
    starts_with_prefix_and_space (sc .class_reserved_word) &&
    ! _contains_the_equals_symbol

  private lazy val _contains_the_equals_symbol : Boolean =
    FunctionDefinitionAnnotation_ (block) .contains_the_equals_symbol

  private lazy val _contains_an_opening_parenthesis : Boolean =
    first_readable_line .line .contains (sc .opening_parenthesis_symbol)

  private lazy val _class_name_and_type_parameters : String =
    skip_first_word (first_readable_line .line)

  lazy val class_name : String =
    get_first_word (_class_name_and_type_parameters)

  lazy val type_parameters_and_bounds : Seq [String] =
    remove_brackets (skip_first_word (_class_name_and_type_parameters) )
      .split (sc .parameter_separation_regex)
      .toIndexedSeq
      .map ( parameter => parameter .trim)
      .filter ( parameter => ! parameter .isEmpty)

  lazy val type_parameters : Seq [String] =
    type_parameters_and_bounds
       .map ( parameter => get_first_word (parameter) )

  def remove_brackets (text : String) : String =
    remove_brackets_with (text .trim)

  def remove_brackets_with (trimmed_text : String) : String =
    if ( trimmed_text .startsWith (sc .opening_bracket_symbol) &&
      trimmed_text .endsWith (sc .closing_bracket_symbol)
    ) trimmed_text .substring (sc .opening_bracket_symbol .length, trimmed_text .length - sc .closing_bracket_symbol .length)
    else trimmed_text

  lazy val is_concrete : Boolean = applies && _contains_an_opening_parenthesis

}

case class ClassBeginningAnnotation_ (block : soda.translator.block.Block) extends ClassBeginningAnnotation


trait ClassEndAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block
  def   references : Seq [soda.translator.block.AnnotatedBlock]

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_end

  lazy val applies : Boolean =
    block .readable_lines .nonEmpty &&
    (block .readable_lines .head .line .trim == SodaConstant_ () .class_end_reserved_word)

}

case class ClassEndAnnotation_ (block : soda.translator.block.Block, references : Seq [soda.translator.block.AnnotatedBlock]) extends ClassEndAnnotation


trait CommentAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_

  lazy val identifier = BlockAnnotationEnum_ () .comment

  lazy val applies : Boolean =
    block
      .annotated_lines
      .forall ( annotated_line => annotated_line .is_comment)

}

case class CommentAnnotation_ (block : soda.translator.block.Block) extends CommentAnnotation


trait FunctionDefinitionAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.replacement.ParserStateEnum_
  import   soda.translator.replacement.Tokenizer_

  lazy val identifier = BlockAnnotationEnum_ () .function_definition

  lazy val sc = SodaConstant_ ()

  private lazy val _symbol_at_the_end : String =
    sc .space +
    sc .function_definition_symbol

  private lazy val _symbol_in_the_middle : String =
    sc .space +
    sc .function_definition_symbol +
    sc .space

  private lazy val _plain_state = ParserStateEnum_ () .plain

  lazy val applies : Boolean =
    ! is_a_theorem &&
    ! is_a_proof &&
    ! is_a_class_declaration &&
    (contains_the_equals_symbol || _starts_with_valid_annotation)

  lazy val contains_the_equals_symbol : Boolean =
    block .readable_lines .nonEmpty &&
    block .readable_lines
      .filter ( annotated_line => ! annotated_line .is_comment)
      .exists ( annotated_line => _contains_the_equals_symbol_in_line (annotated_line .line) )

  private def _contains_the_equals_symbol_in_line (line : String) : Boolean =
    Tokenizer_ (line)
      .tokens
      .exists ( token =>
        token .parser_state == _plain_state &&
        _contains_the_equals_symbol_in_token (token .text)
      )

  private def _contains_the_equals_symbol_in_token (token_text : String) : Boolean =
    (
      (token_text .contains (_symbol_in_the_middle) ) ||
      (token_text .endsWith (_symbol_at_the_end) )
    )

  private lazy val _starts_with_valid_annotation : Boolean =
    block .readable_lines .nonEmpty &&
    _starts_with_valid_annotation_with (block .readable_lines .head .line .trim)

  private def _starts_with_valid_annotation_with (first_line_trimmed : String) : Boolean =
    ( first_line_trimmed == sc .tail_recursion_annotation ||
      first_line_trimmed == sc .override_annotation )

  lazy val is_a_class_declaration : Boolean =
    starts_with_prefix_and_space (sc .class_reserved_word)

  lazy val is_a_theorem : Boolean =
    block .readable_lines .nonEmpty &&
    (block .readable_lines .head .line .trim == SodaConstant_ () .theorem_reserved_word)

  lazy val is_a_proof : Boolean =
    block .readable_lines .nonEmpty &&
    (block .readable_lines .head .line .trim == SodaConstant_ () .proof_reserved_word)

}

case class FunctionDefinitionAnnotation_ (block : soda.translator.block.Block) extends FunctionDefinitionAnnotation


trait ImportDeclarationAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .import_declaration

  lazy val applies : Boolean =
    block .readable_lines .nonEmpty &&
    (block .readable_lines .head .line .trim == SodaConstant_ () .import_reserved_word)

  lazy val imported_items : Seq [AnnotatedLine] =
    content_lines

}

case class ImportDeclarationAnnotation_ (block : soda.translator.block.Block) extends ImportDeclarationAnnotation


trait PackageDeclarationAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .package_declaration

  lazy val applies : Boolean =
    starts_with_prefix_and_space (SodaConstant_ () .package_reserved_word)

}

case class PackageDeclarationAnnotation_ (block : soda.translator.block.Block) extends PackageDeclarationAnnotation


trait ProofBlockAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .proof_block

  lazy val applies : Boolean =
    block .readable_lines .nonEmpty &&
    (block .readable_lines .head .line .trim == SodaConstant_ () .proof_reserved_word)

}

case class ProofBlockAnnotation_ (block : soda.translator.block.Block) extends ProofBlockAnnotation


trait TestDeclarationAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .test_declaration

  lazy val applies : Boolean =
    starts_with_prefix_and_space (SodaConstant_ () .test_special_function)

}

case class TestDeclarationAnnotation_ (block : soda.translator.block.Block) extends TestDeclarationAnnotation


trait TheoremBlockAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .theorem_block

  lazy val applies : Boolean =
    block .readable_lines .nonEmpty &&
    (block .readable_lines .head .line .trim == SodaConstant_ () .theorem_reserved_word)

}

case class TheoremBlockAnnotation_ (block : soda.translator.block.Block) extends TheoremBlockAnnotation

