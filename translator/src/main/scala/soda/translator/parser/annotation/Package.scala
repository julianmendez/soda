package soda.translator.parser.annotation

/*
 * This package contains classes to handle block annotations for parsing.
 */





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

object AbstractDeclarationAnnotation {
  def mk (block : soda.translator.block.Block) (references : Seq [soda.translator.block.AnnotatedBlock]) : AbstractDeclarationAnnotation =
    AbstractDeclarationAnnotation_ (block, references)
}


trait AnnotationFactory
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedBlock_
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.Block
  import   soda.translator.block.BlockAnnotationId
  import   soda.translator.block.BlockAnnotationEnum_

  private def _mk_AnnotatedBlock (annotated_lines : Seq [AnnotatedLine] )
      (block_annotation : BlockAnnotationId) : AnnotatedBlock =
    AnnotatedBlock_ (annotated_lines, block_annotation)

  def update_block (original_content : AnnotatedBlock) (new_content : Block) : AnnotatedBlock =
    original_content match  {
      case FunctionDefinitionAnnotation_ (b) => FunctionDefinitionAnnotation_ (new_content)
      case ClassBeginningAnnotation_ (b) => ClassBeginningAnnotation_ (new_content)
      case ClassEndAnnotation_ (b, references) =>
        ClassEndAnnotation_ (new_content, references)
      case AbstractDeclarationAnnotation_ (b, references) =>
        AbstractDeclarationAnnotation_ (new_content, references)
      case ImportDeclarationAnnotation_ (b) => ImportDeclarationAnnotation_ (new_content)
      case PackageDeclarationAnnotation_ (b) => PackageDeclarationAnnotation_ (new_content)
      case ClassAliasAnnotation_ (b) => ClassAliasAnnotation_ (new_content)
      case TheoremBlockAnnotation_ (b) => TheoremBlockAnnotation_ (new_content)
      case DirectiveBlockAnnotation_ (b) => DirectiveBlockAnnotation_ (new_content)
      case CommentAnnotation_ (b) => CommentAnnotation_ (new_content)
      case TestDeclarationAnnotation_ (b) => TestDeclarationAnnotation_ (new_content)
      case _otherwise =>
        _mk_AnnotatedBlock (new_content .annotated_lines) (original_content .block_annotation)
    }

  private def _detectors (block : Block) : Seq [BlockAnnotationParser] =
    Seq (
      FunctionDefinitionAnnotation_ (block),
      ClassBeginningAnnotation_ (block),
      ClassEndAnnotation_ (block, Seq [BlockAnnotationParser] () ),
      AbstractDeclarationAnnotation_ (block, Seq [BlockAnnotationParser] () ),
      ImportDeclarationAnnotation_ (block),
      PackageDeclarationAnnotation_ (block),
      ClassAliasAnnotation_ (block),
      TheoremBlockAnnotation_ (block),
      DirectiveBlockAnnotation_ (block),
      CommentAnnotation_ (block),
      TestDeclarationAnnotation_ (block)
    )

  private def _find_candidates (block : Block) : Seq [BlockAnnotationParser] =
    _detectors (block)
      .filter ( detector => detector .applies)

  private def _get_first_or_undefined (candidates : Seq [BlockAnnotationParser] ) (block : Block)
      : AnnotatedBlock =
    if ( candidates .length == 1
    ) candidates .head
    else _mk_AnnotatedBlock (block .annotated_lines) (BlockAnnotationEnum_ () .undefined )

  def annotate (block : Block) : AnnotatedBlock =
    block match  {
      case AnnotatedBlock_ (annotated_lines, block_annotation) =>
        _mk_AnnotatedBlock (annotated_lines) (block_annotation)
      case _otherwise => _get_first_or_undefined (_find_candidates (block) ) (block)
    }

  def translate_for (block : AnnotatedBlock) : AnnotatedBlock =
    if ( block .block_annotation == BlockAnnotationEnum_ () .undefined
    ) annotate (block)
    else block

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class AnnotationFactory_ () extends AnnotationFactory

object AnnotationFactory {
  def mk : AnnotationFactory =
    AnnotationFactory_ ()
}


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

  private def _get_first_word_with (index : Int) (line : String) : String =
    if ( index >= 0
    ) line .substring (0, index)
    else line

  def get_first_word (line : String) : String =
    (_get_first_word_with (line .trim .indexOf (space) ) (line) ) .trim

  private def _skip_first_word_with (index : Int) (line : String) : String =
    if ( index >= 0
    ) line .trim .substring (index)
    else ""

  def skip_first_word (line : String) : String =
    (_skip_first_word_with (line .trim .indexOf (space) ) (line) ) .trim

}

case class BlockAnnotationParser_ (block : soda.translator.block.Block, applies : Boolean, identifier : soda.translator.block.BlockAnnotationId) extends BlockAnnotationParser

object BlockAnnotationParser {
  def mk (block : soda.translator.block.Block) (applies : Boolean) (identifier : soda.translator.block.BlockAnnotationId) : BlockAnnotationParser =
    BlockAnnotationParser_ (block, applies, identifier)
}


trait ClassAliasAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_alias

  lazy val sc = SodaConstant_ ()

  private lazy val _contains_the_equals_symbol : Boolean =
    FunctionDefinitionAnnotation_ (block) .contains_the_equals_symbol

  lazy val applies : Boolean =
     starts_with_prefix_and_space (sc .class_reserved_word) &&
     _contains_the_equals_symbol

}

case class ClassAliasAnnotation_ (block : soda.translator.block.Block) extends ClassAliasAnnotation

object ClassAliasAnnotation {
  def mk (block : soda.translator.block.Block) : ClassAliasAnnotation =
    ClassAliasAnnotation_ (block)
}


trait ClassBeginningAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_beginning

  lazy val sc = SodaConstant_ ()

  private lazy val _contains_the_equals_symbol : Boolean =
    FunctionDefinitionAnnotation_ (block) .contains_the_equals_symbol

  private lazy val _contains_an_opening_parenthesis : Boolean =
    first_readable_line .line .contains (sc .opening_parenthesis_symbol)

  private def _remove_type_annotation_in_line (line : String) : String =
    line .replaceAll (sc .main_type_membership_regex , "")

  private lazy val _class_name_and_type_parameters : String =
    skip_first_word (_remove_type_annotation_in_line (first_readable_line .line) )

  lazy val class_name : String =
    get_first_word (_class_name_and_type_parameters)

  def remove_brackets_with (trimmed_text : String) : String =
    if ( trimmed_text .startsWith (sc .opening_bracket_symbol) &&
      trimmed_text .endsWith (sc .closing_bracket_symbol)
    ) trimmed_text .substring (sc .opening_bracket_symbol .length,
      trimmed_text .length - sc .closing_bracket_symbol .length)
    else trimmed_text

  def remove_brackets (text : String) : String =
    remove_brackets_with (text .trim)

  lazy val type_parameters_and_bounds : Seq [String] =
    remove_brackets (skip_first_word (_class_name_and_type_parameters) )
      .split (sc .type_parameter_separation_regex)
      .toIndexedSeq
      .map ( parameter => parameter .trim)
      .filter ( parameter => ! parameter .isEmpty)

  lazy val applies : Boolean =
    starts_with_prefix_and_space (sc .class_reserved_word) &&
    ! _contains_the_equals_symbol

  lazy val is_concrete : Boolean = applies && _contains_an_opening_parenthesis

  lazy val type_parameters : Seq [String] =
    type_parameters_and_bounds
       .map ( parameter => get_first_word (parameter) )

}

case class ClassBeginningAnnotation_ (block : soda.translator.block.Block) extends ClassBeginningAnnotation

object ClassBeginningAnnotation {
  def mk (block : soda.translator.block.Block) : ClassBeginningAnnotation =
    ClassBeginningAnnotation_ (block)
}


trait ClassEndAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block
  def   references : Seq [soda.translator.block.AnnotatedBlock]

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_end

  private lazy val _sc = SodaConstant_ ()

  private def _get_first_word_of_array (words : Array [String] ) : String =
    if ( words .size == 0
    ) ""
    else words .apply (0)

  private def _get_first_word (line : String) : String =
    _get_first_word_of_array (line .split (" ") )

  lazy val applies : Boolean =
    block .readable_lines .nonEmpty &&
    (_get_first_word (block .readable_lines .head .line .trim) == _sc .class_end_reserved_word)

}

case class ClassEndAnnotation_ (block : soda.translator.block.Block, references : Seq [soda.translator.block.AnnotatedBlock]) extends ClassEndAnnotation

object ClassEndAnnotation {
  def mk (block : soda.translator.block.Block) (references : Seq [soda.translator.block.AnnotatedBlock]) : ClassEndAnnotation =
    ClassEndAnnotation_ (block, references)
}


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

object CommentAnnotation {
  def mk (block : soda.translator.block.Block) : CommentAnnotation =
    CommentAnnotation_ (block)
}


trait DirectiveBlockAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .directive_block

  private lazy val _sc = SodaConstant_ ()

  private def _get_first_line_or_empty (annotated_lines : Seq [AnnotatedLine] ) : String =
    annotated_lines match  {
      case x +: xs => x .line
      case Nil => ""
    }

  lazy val applies : Boolean =
    (_get_first_line_or_empty (block .readable_lines) .trim + _sc .space)
      .startsWith (_sc .directive_reserved_word + _sc .space)

}

case class DirectiveBlockAnnotation_ (block : soda.translator.block.Block) extends DirectiveBlockAnnotation

object DirectiveBlockAnnotation {
  def mk (block : soda.translator.block.Block) : DirectiveBlockAnnotation =
    DirectiveBlockAnnotation_ (block)
}


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

  private def _contains_the_equals_symbol_in_token (token_text : String) : Boolean =
    (
      (token_text .contains (_symbol_in_the_middle) ) ||
      (token_text .endsWith (_symbol_at_the_end) )
    )

  private def _contains_the_equals_symbol_in_line (line : String) : Boolean =
    Tokenizer_ (line)
      .tokens
      .exists ( token =>
        token .parser_state == _plain_state &&
        _contains_the_equals_symbol_in_token (token .text)
      )

  lazy val contains_the_equals_symbol : Boolean =
    block .readable_lines .nonEmpty &&
    block .readable_lines
      .filter ( annotated_line => ! annotated_line .is_comment)
      .exists ( annotated_line =>
        _contains_the_equals_symbol_in_line (annotated_line .line) )

  private def _starts_with_valid_annotation_with (first_line_trimmed : String) : Boolean =
    (first_line_trimmed == sc .tail_recursion_annotation ||
    first_line_trimmed == sc .override_annotation )

  lazy val starts_with_valid_annotation : Boolean =
    block .readable_lines .nonEmpty &&
    _starts_with_valid_annotation_with (block .readable_lines .head .line .trim)

  private def _starts_with_def_reserved_word_with (first_line_trimmed : String) : Boolean =
    first_line_trimmed .startsWith (sc .def_reserved_word + sc .space)

  lazy val starts_with_def_reserved_word : Boolean =
    block .readable_lines .nonEmpty &&
    _starts_with_def_reserved_word_with (block .readable_lines .head .line .trim)

  lazy val is_a_class_declaration : Boolean =
    starts_with_prefix_and_space (sc .class_reserved_word)

  lazy val is_a_theorem : Boolean =
    TheoremBlockAnnotation_ (block) .applies

  lazy val is_a_directive : Boolean =
    DirectiveBlockAnnotation_ (block) .applies

  lazy val applies : Boolean =
    ! is_a_theorem &&
    ! is_a_directive &&
    ! is_a_class_declaration &&
    (contains_the_equals_symbol || starts_with_valid_annotation ||
      starts_with_def_reserved_word)

}

case class FunctionDefinitionAnnotation_ (block : soda.translator.block.Block) extends FunctionDefinitionAnnotation

object FunctionDefinitionAnnotation {
  def mk (block : soda.translator.block.Block) : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation_ (block)
}


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

object ImportDeclarationAnnotation {
  def mk (block : soda.translator.block.Block) : ImportDeclarationAnnotation =
    ImportDeclarationAnnotation_ (block)
}


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

object PackageDeclarationAnnotation {
  def mk (block : soda.translator.block.Block) : PackageDeclarationAnnotation =
    PackageDeclarationAnnotation_ (block)
}


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

object TestDeclarationAnnotation {
  def mk (block : soda.translator.block.Block) : TestDeclarationAnnotation =
    TestDeclarationAnnotation_ (block)
}


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

object TheoremBlockAnnotation {
  def mk (block : soda.translator.block.Block) : TheoremBlockAnnotation =
    TheoremBlockAnnotation_ (block)
}

