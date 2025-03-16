package soda.translator.extension.tolean

/*
 * This package contains classes for the translation to Lean.
 */







trait LeanClassAliasBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.block.Translator
  import   soda.translator.blocktr.TableTranslator_
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.SodaConstant
  import   soda.translator.parser.annotation.ClassAliasAnnotation
  import   soda.translator.parser.annotation.ClassAliasAnnotation_
  import   soda.translator.replacement.Replacement_

  private lazy val _sc = SodaConstant .mk

  private lazy val _tc = TranslationConstantToLean .mk

  def get_first_line (block : Block) : String =
    block .lines .headOption .getOrElse ("")

  private def _process_class_alias (line : String) : String =
    line
      .replace (_sc .class_alias_reserved_word + _sc .space , _tc .lean_notation_prefix)
      .replace (_sc .space + _sc .class_alias_definition_symbol + _sc .space ,
         _tc .lean_notation_infix)

  private def _translate_block (block : AnnotatedBlock) : Block =
    BlockBuilder .mk .build (
      Seq [String] (
        _process_class_alias (get_first_line (block) )
      )
    )

  private def _translate_class_alias_block (block : ClassAliasAnnotation)
      : ClassAliasAnnotation =
    ClassAliasAnnotation .mk (_translate_block (block) )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassAliasAnnotation_ (block) =>
        _translate_class_alias_block (ClassAliasAnnotation .mk (block) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanClassAliasBlockTranslator_ () extends LeanClassAliasBlockTranslator

object LeanClassAliasBlockTranslator {
  def mk : LeanClassAliasBlockTranslator =
    LeanClassAliasBlockTranslator_ ()
}


trait LeanClassConstructorBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.SodaConstant
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_

  private lazy val _sc = SodaConstant .mk

  private lazy val _tc = TranslationConstantToLean .mk

  private def _get_as_class_beginning_annotation (annotated_block : AnnotatedBlock)
      : Option [ClassBeginningAnnotation] =
    annotated_block match  {
      case ClassBeginningAnnotation_ (b) => Some (ClassBeginningAnnotation .mk (b) )
      case _otherwise => None
    }

  private def _get_class_beginning (references : Seq [AnnotatedBlock] )
      : Option [ClassBeginningAnnotation] =
    references
       .flatMap ( block => _get_as_class_beginning_annotation (block) )
       .headOption

  private def _translate_type_symbols (line : String) : String =
    line
      .replaceAll (_sc .subtype_reserved_word , _tc .lean_subtype_symbol)
      .replaceAll (_sc .supertype_reserved_word , _tc .lean_supertype_symbol)
      .replaceAll (_sc .function_arrow_symbol , _tc .lean_function_arrow_symbol)

  private def _get_types_of_abstract_functions (block : AbstractDeclarationAnnotation) : Seq [String] =
    block .abstract_functions
      .map ( annotated_line => _translate_type_symbols (annotated_line .line) .trim )

  private def _get_first_line (block : AnnotatedBlock) : String =
    block .lines .headOption .getOrElse ("")

  private def _get_initial_spaces_with (line : String) : String =
    line .takeWhile ( ch => ch .isSpaceChar)

  private def _get_initial_spaces (block : AnnotatedBlock) : String =
    _get_initial_spaces_with (_get_first_line (block) )

  private lazy val _two_spaces : String = _tc .lean_space + _tc .lean_space

  private lazy val _four_spaces : String = _two_spaces + _two_spaces

  private def _get_type_parameters_of_class (beginning : ClassBeginningAnnotation) : String =
    ""

  private def _get_constructor_declaration (beginning : ClassBeginningAnnotation)
     (functions : Seq [String] ) : String =
    _get_initial_spaces (beginning) +
    _tc .lean_where_reserved_word +
    _tc .lean_new_line + _get_initial_spaces (beginning) + _two_spaces +
    _tc .lean_default_constructor_name +
    _tc .lean_space +
    _tc .lean_list_constructor_symbol +
    _tc .lean_new_line + _get_initial_spaces (beginning) + _four_spaces +
    functions .mkString (_tc .lean_new_line + _get_initial_spaces (beginning) + _four_spaces) +
    _tc .lean_new_line +
    _get_initial_spaces (beginning) + _two_spaces +
    _tc .lean_deriving_reserved_word + _tc .lean_space + _tc .lean_decidable_eq_type_name +
    _tc .lean_new_line +
    _tc .lean_new_line + _get_initial_spaces (beginning) +
    _tc .lean_namespace_reserved_word + _tc .lean_space + beginning .class_name +
    _tc .lean_new_line

  private def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation)
      (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    AbstractDeclarationAnnotation .mk (
      BlockBuilder .mk .build (
        Seq [String] (
          _get_constructor_declaration (beginning) (_get_types_of_abstract_functions (block) ) )
      )
    ) (block .references)

  private def _translate_block_with_beginning (beginning : ClassBeginningAnnotation)
      (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    if ( beginning .is_concrete
    ) block
    else _translate_block_with_abstract_beginning (beginning) (block)

  private def _translate_block_with (maybe_beginning : Option [ClassBeginningAnnotation] )
      (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    if ( maybe_beginning .isEmpty
    ) block
    else _translate_block_with_beginning (maybe_beginning .get) (block)

  private def _translate_block (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    _translate_block_with (_get_class_beginning (block .references) ) (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case AbstractDeclarationAnnotation_ (block , references) =>
        _translate_block (AbstractDeclarationAnnotation .mk (block) (references) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanClassConstructorBlockTranslator_ () extends LeanClassConstructorBlockTranslator

object LeanClassConstructorBlockTranslator {
  def mk : LeanClassConstructorBlockTranslator =
    LeanClassConstructorBlockTranslator_ ()
}


trait LeanClassDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.block.Translator
  import   soda.translator.blocktr.TableTranslator_
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.SodaConstant
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.replacement.Replacement_

  private lazy val _sc = SodaConstant .mk

  private lazy val _tc = TranslationConstantToLean .mk

  lazy val soda_space : String = _sc .space

  lazy val scala_space : String = " "

  def get_first_line (lines : Seq [String] ) : String =
    lines .headOption .getOrElse ("")

  def get_initial_spaces_for (line : String) : String =
    line .takeWhile ( ch => ch .isSpaceChar)

  def get_initial_spaces (lines : Seq [String] ) : String =
    get_initial_spaces_for (get_first_line (lines) )

  private def _process_after_extends (lines : Seq [String] ) : Seq [String] =
    if ( (get_first_line (lines) .trim .nonEmpty)
    ) (Seq [String] () .:+ (_tc .lean_line_comment + _tc .lean_space +
      _tc .lean_extends_reserved_word + _tc .lean_space +
      lines .mkString (_tc .lean_comma_symbol) ) )
    else Seq [String] ()

  def remove_first_line (lines : Seq [String] ) : Seq [String] =
    if ( lines .isEmpty
    ) lines
    else lines .tail

  private def _process_if_extends (lines : Seq [String] ) : Seq [String] =
    if ( (get_first_line (lines) .trim == _sc .extends_reserved_word)
    ) Seq [String] (get_initial_spaces (lines) ) .++ (
      _process_after_extends (remove_first_line (lines) ) )
    else lines

  private def _process_tail (lines : Seq [String] ) : Seq [String] =
    _process_if_extends (remove_first_line (lines) )

  def get_table_translator (line : String) : Translator =
    TableTranslator_ (
      Seq (Tuple2 (_sc .class_reserved_word, _tc .lean_class_reserved_word) )
    )

  private def _process_head_with (line : String) : Seq [String] =
    Seq [String] (
      Replacement_ (_sc .space + line)
        .replace_at_beginning (0) (get_table_translator (line) )
        .line .substring (_sc .space .length)
    )

  private def _process_head (lines : Seq [String] ) : Seq [String] =
    _process_head_with (get_first_line (lines) )

  def contains_equals (line : String) : Boolean =
    line .trim .contains (_sc .function_definition_symbol)

  def has_condition_for_type_alias (line : String) : Boolean =
    contains_equals (line)

  private def _translate_block (block : AnnotatedBlock) : Block =
    if ( (has_condition_for_type_alias (get_first_line (block . lines) ) )
    ) block
    else
      BlockBuilder .mk .build (
        _process_head (block .lines) .++ (_process_tail (block .lines) )
      )

  private def _translate_class_beginning_block (block : ClassBeginningAnnotation)
      : ClassBeginningAnnotation =
    ClassBeginningAnnotation_ (_translate_block (block) )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassBeginningAnnotation_ (block) =>
        _translate_class_beginning_block (ClassBeginningAnnotation_ (block) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanClassDeclarationBlockTranslator_ () extends LeanClassDeclarationBlockTranslator

object LeanClassDeclarationBlockTranslator {
  def mk : LeanClassDeclarationBlockTranslator =
    LeanClassDeclarationBlockTranslator_ ()
}


trait LeanClassEndBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.SodaConstant
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  private lazy val _sc = SodaConstant .mk

  private lazy val _tc = TranslationConstantToLean .mk

  private def _mk_ClassEndAnnotation (block : Block) (references : Seq [AnnotatedBlock] )
      : ClassEndAnnotation =
    ClassEndAnnotation_ (block, references)

  private def _get_between_quotes (text : String) : String =
     _tc .lean_quotes_symbol + text + _tc .lean_quotes_symbol

  private def _constructor_name (class_name : String) : String =
    class_name + _sc .constructor_suffix

  private def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation)
      (block : ClassEndAnnotation) : ClassEndAnnotation =
    _mk_ClassEndAnnotation (
      BlockBuilder .mk .build (
        Seq [String] (
          _tc .lean_namespace_end_reserved_word + _tc .lean_space + beginning .class_name ,
          "",
          _tc .lean_notation_reserved_word + _tc .lean_space +
          _get_between_quotes (_constructor_name (beginning .class_name) ) +
          _tc .lean_space + _tc .lean_notation_arrow_symbol + _tc .lean_space +
          beginning .class_name + _tc .lean_dot_notation_symbol +
          _tc .lean_default_constructor_name
        )
      ) ) (
      block .references
    )

  private def _translate_block_with_beginning (beginning : ClassBeginningAnnotation)
      (block : ClassEndAnnotation) : ClassEndAnnotation =
    if ( beginning .is_concrete
    ) block
    else _translate_block_with_abstract_beginning (beginning) (block)

  private def _translate_block_with (maybe_beginning : Option [ClassBeginningAnnotation] )
      (block : ClassEndAnnotation) : ClassEndAnnotation =
    if ( maybe_beginning .isEmpty
    ) block
    else _translate_block_with_beginning (maybe_beginning .get) (block)

  private def _get_class_beginning (references : Seq [AnnotatedBlock] )
      : Option [ClassBeginningAnnotation] =
    references
      .flatMap ( block => _get_as_class_beginning_annotation (block) )
      .headOption

  private def _get_as_class_beginning_annotation (annotated_block : AnnotatedBlock)
      : Option [ClassBeginningAnnotation] =
    annotated_block match  {
      case ClassBeginningAnnotation_ (b) => Some (ClassBeginningAnnotation_ (b) )
      case _otherwise => None
    }

  private def _translate_block (block : ClassEndAnnotation) : ClassEndAnnotation =
    _translate_block_with (_get_class_beginning (block.references) ) (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassEndAnnotation_ (block, references) =>
        _translate_block (_mk_ClassEndAnnotation (block) (references) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanClassEndBlockTranslator_ () extends LeanClassEndBlockTranslator

object LeanClassEndBlockTranslator {
  def mk : LeanClassEndBlockTranslator =
    LeanClassEndBlockTranslator_ ()
}


trait LeanDirectiveBlockTranslator
  extends
    soda.translator.blocktr.DirectiveBlockTranslator
{



  private lazy val _tc = TranslationConstantToLean .mk

  lazy val identifier : String = _tc .lean_directive_identifier

  lazy val opening_comment : String = _tc .lean_comment_opening_symbol

  lazy val closing_comment : String = _tc .lean_comment_closing_symbol

}

case class LeanDirectiveBlockTranslator_ () extends LeanDirectiveBlockTranslator

object LeanDirectiveBlockTranslator {
  def mk : LeanDirectiveBlockTranslator =
    LeanDirectiveBlockTranslator_ ()
}


trait LeanDocumentationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.SodaConstant
  import   soda.translator.parser.annotation.CommentAnnotation
  import   soda.translator.parser.annotation.CommentAnnotation_
  import   soda.translator.parser.tool.CommentDelimiterRemover

  private lazy val _sc = SodaConstant .mk

  private lazy val _tc = TranslationConstantToLean .mk

  private lazy val _comment_line_prefix = _sc .comment_line_symbol + _sc .space

  private def _prepend (prefix : String) (content : Seq [String] ) : Seq [String] =
    if ( content .isEmpty
    ) Seq [String] (prefix)
    else Seq [String] (prefix + content .head) .++ (content .tail)

  private def _append (suffix : String) (content : Seq [String] ) : Seq [String] =
    content .:+ (suffix)

  private def _translate_with_symbol (opening_symbol : String) (lines : Seq [String] ) : Seq [String] =
    _append (
      _tc .lean_comment_closing_symbol) (
      _prepend (opening_symbol) (
        CommentDelimiterRemover .mk
          .remove_comment_delimiters (lines)
      )
    )

  private def _get_opening_symbol (line : String) : String =
    if ( (line .trim .startsWith (_sc .documentation_comment_opening_symbol) )
    ) _tc .lean_opening_documentation
    else _tc .lean_comment_opening_symbol

  private def _translate_lines (lines : Seq [String] ) : Seq [String] =
    if ( (lines .isEmpty)
    ) lines
    else _translate_with_symbol (_get_opening_symbol (lines .head) ) (lines)

  private def _translate_comment (block : CommentAnnotation) : CommentAnnotation =
    CommentAnnotation_ (
      BlockBuilder .mk .build (
        _translate_lines (block .lines)
      )
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case CommentAnnotation_ (block) => _translate_comment (CommentAnnotation_ (block) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanDocumentationBlockTranslator_ () extends LeanDocumentationBlockTranslator

object LeanDocumentationBlockTranslator {
  def mk : LeanDocumentationBlockTranslator =
    LeanDocumentationBlockTranslator_ ()
}


trait LeanDotNotationBlockTranslator
  extends
    soda.translator.blocktr.TokenizedBlockTranslator
{



  import   soda.translator.parser.SodaConstant
  import   soda.translator.replacement.Token

  private lazy val _sc = SodaConstant .mk

  private lazy val _tc = TranslationConstantToLean .mk

  lazy val replace_token : Token => String =
     token =>
      token
        .text
        .replaceAll (_sc .dot_notation_regex , _tc .lean_dot_notation_symbol)

}

case class LeanDotNotationBlockTranslator_ () extends LeanDotNotationBlockTranslator

object LeanDotNotationBlockTranslator {
  def mk : LeanDotNotationBlockTranslator =
    LeanDotNotationBlockTranslator_ ()
}


trait LeanFunctionDefinitionBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.SodaConstant
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_

  private lazy val _sc = SodaConstant .mk

  private lazy val _tc = TranslationConstantToLean .mk

  private lazy val _soda_def_prefix = _sc .def_reserved_word + _sc .space

  private lazy val _empty_string = ""

  private def _append (suffix : String) (block : Block) : Block =
    BlockBuilder .mk .build (
      block .lines .:+ (suffix)
    )

  private def _prepend (prefix : String) (block : Block) : Block =
    BlockBuilder .mk .build (
      Seq [String] (prefix + block .lines .head) ++ block .lines .tail
    )

  def is_a_definition (block : Block) : Boolean =
    ! is_a_recursive_definition (block) &&
    ! _tc .non_definition_block_prefixes .exists ( prefix =>
      block .contents .trim .startsWith (prefix) )

  def first_line (block : Block) : String =
    block .lines .headOption .getOrElse (_empty_string) .trim

  def is_private (block :  FunctionDefinitionAnnotation) : Boolean =
    first_line (block) .trim .startsWith (_sc .private_function_prefix)

  private def _private_prefix_if_necessary (block : FunctionDefinitionAnnotation) : String =
    if ( is_private (block)
    ) _tc .lean_private_reserved_word + _tc .lean_space
    else _empty_string

  private def _translate_non_recursive_definition (block : FunctionDefinitionAnnotation) : Block =
    if ( is_a_definition (block)
    ) _append (_tc .lean_definition_end_symbol) (
      _prepend (
        _private_prefix_if_necessary (block) +
        _tc .lean_def_reserved_word + _tc .lean_space) (block) )
    else block

  private def _translate_recursive_definition (block : FunctionDefinitionAnnotation) : Block =
    _append (_tc .lean_recursive_definition_end_symbol) (_prepend (
      _private_prefix_if_necessary (block) +
      _tc .lean_recursive_definition_reserved_word + _tc .lean_space) (block) )

  def is_a_recursive_definition (block : Block) : Boolean =
    _tc .lean_recursive_function_prefixes .exists ( prefix =>
      first_line (block) .startsWith (prefix) )

  private def _remove_part_at (line : String) (from : Int) (length : Int) : String =
    if ( (from >= 0)
    ) line .substring (0 , from) + line .substring (from + length)
    else line

  private def _remove_part (line : String) (part : String) : String =
    _remove_part_at (line) (line .indexOf (part) ) (part .length)

  private def _remove_def_if_present (block : FunctionDefinitionAnnotation) : FunctionDefinitionAnnotation =
    if ( (block .lines .nonEmpty) &&
      (block .lines .head .trim .startsWith (_soda_def_prefix) )
    )
      FunctionDefinitionAnnotation .mk (
        BlockBuilder .mk .build (
        (Seq [String] ()
          .+: (_remove_part (block .lines .head) (_soda_def_prefix) ) )
          .++ (block .lines .tail)
        )
      )
    else block

  private def _translate_block_with (block : FunctionDefinitionAnnotation) : Block =
    if ( is_a_recursive_definition (block)
    ) _translate_recursive_definition (block)
    else _translate_non_recursive_definition (block)

  private def _translate_block (block : FunctionDefinitionAnnotation) : Block =
    _translate_block_with (_remove_def_if_present (block) )

  private def _translate_definition_block (block : FunctionDefinitionAnnotation)
      : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation .mk (_translate_block (block) )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case FunctionDefinitionAnnotation_ (block) =>
        _translate_definition_block (FunctionDefinitionAnnotation .mk (block) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanFunctionDefinitionBlockTranslator_ () extends LeanFunctionDefinitionBlockTranslator

object LeanFunctionDefinitionBlockTranslator {
  def mk : LeanFunctionDefinitionBlockTranslator =
    LeanFunctionDefinitionBlockTranslator_ ()
}


trait LeanImportDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation_

  private lazy val _tc = TranslationConstantToLean .mk

  lazy val lean_import_declaration_pattern =
    _tc .lean_import_reserved_word + _tc .lean_space

  def prepend_aligned_non_comment (index : Int) (prefix : String) (annotated_line : AnnotatedLine)
      : String =
    if ( annotated_line .is_comment
    ) annotated_line .line
    else annotated_line .line .substring (0, index) + prefix +
      annotated_line .line .substring (index)

  def prepend_to_lines_aligned_at (number_of_spaces : Int) (prefix : String)
      (annotated_lines : Seq [AnnotatedLine] ) : Block =
    BlockBuilder .mk .build (
      annotated_lines .map ( annotated_line =>
        prepend_aligned_non_comment (number_of_spaces) (prefix) (annotated_line) )
    )

  def get_number_of_spaces_at_beginning (line : String) : Int =
    line
      .takeWhile ( ch => ch .isSpaceChar)
      .length

  def get_first_line (block : AnnotatedBlock) : String =
    block .lines .headOption .getOrElse ("")

  private def _translate_block (block : ImportDeclarationAnnotation) : ImportDeclarationAnnotation =
    ImportDeclarationAnnotation_ (
      prepend_to_lines_aligned_at (
        get_number_of_spaces_at_beginning (get_first_line (block) ) ) (
        lean_import_declaration_pattern) (
        block .imported_items
          .filter( annotated_line => ! annotated_line .is_comment)
          .map ( annotated_line => AnnotatedLine_ (
            annotated_line .line + _tc .lean_space + _tc .lean_end_symbol, false) )
      )
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ImportDeclarationAnnotation_ (block) =>
        _translate_block (ImportDeclarationAnnotation_ (block) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanImportDeclarationBlockTranslator_ () extends LeanImportDeclarationBlockTranslator

object LeanImportDeclarationBlockTranslator {
  def mk : LeanImportDeclarationBlockTranslator =
    LeanImportDeclarationBlockTranslator_ ()
}


trait LeanMatchCaseBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.SodaConstant
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_
  import   soda.translator.parser.annotation.TestDeclarationAnnotation
  import   soda.translator.parser.annotation.TestDeclarationAnnotation_
  import   soda.translator.replacement.ReplacementAux

  private lazy val _sc = SodaConstant .mk

  private lazy val _soda_case_pattern = _sc .case_reserved_word + _sc .space

  private lazy val _tc = TranslationConstantToLean .mk

  private lazy val _soda_match_pattern = _sc .match_reserved_word + " "

  private def _is_a_match_line (line : String) : Boolean =
    line .trim .startsWith (_soda_match_pattern)

  private def _is_a_match_case_structure (block : AnnotatedBlock) : Boolean =
    block .lines .exists ( line => _is_a_match_line (line) )

  private def _append_with_after_match (line : String) : String =
    if ( _is_a_match_line (line)
    ) line + _tc .lean_space + _tc .lean_with_reserved_word
    else line

  private def _is_a_case_line (line : String) : Boolean =
    line .trim .startsWith (_soda_case_pattern)

  private def _replace_case (line : String) : String =
    if ( _is_a_case_line (line)
    )
      ReplacementAux .mk
        .replace_first (line) (_soda_case_pattern) (_tc .lean_case_translation)
    else line

  private def _left_part (index : Int) (line : String) : String =
    line .substring (0 , index)

  private def _get_tabulation_of_match (block : AnnotatedBlock) : String =
    block .lines
      .find ( line => _is_a_match_line (line) )
      .map ( line => _left_part (line .indexOf (_soda_match_pattern) ) (line) )
      .getOrElse (_tc .lean_space)

  private def _translate_match_case_structure (block: AnnotatedBlock) (tabulation : String) : Block =
    BlockBuilder .mk .build (
      block .lines
        .map ( line => _append_with_after_match (line) )
        .map ( line => _replace_case (line) )
        .++ (Seq [String] ()  .+: (tabulation + _tc .lean_match_end_translation) )
    )

  private def _translate_block (block : AnnotatedBlock) : Block =
    if ( _is_a_match_case_structure (block)
    ) _translate_match_case_structure (block) (_get_tabulation_of_match (block) )
    else block

  private def _translate_function_block (block : AnnotatedBlock) : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation .mk (_translate_block (block) )

  private def _translate_test_block (block : AnnotatedBlock) : TestDeclarationAnnotation =
    TestDeclarationAnnotation .mk (_translate_block (block) )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case FunctionDefinitionAnnotation_ (block) =>
        _translate_function_block (FunctionDefinitionAnnotation_ (block) )
      case TestDeclarationAnnotation_ (block) =>
        _translate_test_block (TestDeclarationAnnotation_ (block) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanMatchCaseBlockTranslator_ () extends LeanMatchCaseBlockTranslator

object LeanMatchCaseBlockTranslator {
  def mk : LeanMatchCaseBlockTranslator =
    LeanMatchCaseBlockTranslator_ ()
}


trait LeanPackageDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.PackageDeclarationAnnotation
  import   soda.translator.parser.annotation.PackageDeclarationAnnotation_

  private lazy val _tc = TranslationConstantToLean .mk

  private def _comment_block (block : Block) : Block =
    BlockBuilder .mk .build (
      ( (Seq (_tc .lean_comment_opening_symbol) .++ (block .lines) ) .++ (
        Seq (_tc .lean_comment_closing_symbol) ) ) .++ (_tc .lean_prelude)
    )

  private def _translate_block (block : PackageDeclarationAnnotation) : PackageDeclarationAnnotation =
    PackageDeclarationAnnotation_ (
      _comment_block (
        block
      )
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case PackageDeclarationAnnotation_ (block) =>
        _translate_block (PackageDeclarationAnnotation_ (block) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanPackageDeclarationBlockTranslator_ () extends LeanPackageDeclarationBlockTranslator

object LeanPackageDeclarationBlockTranslator {
  def mk : LeanPackageDeclarationBlockTranslator =
    LeanPackageDeclarationBlockTranslator_ ()
}


trait LeanTheoremBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.TheoremBlockAnnotation
  import   soda.translator.parser.annotation.TheoremBlockAnnotation_

  private lazy val _tc = TranslationConstantToLean .mk

  private def _prepend (prefix : String) (block : Block) : Block =
    BlockBuilder .mk .build (
      Seq [String] (prefix + block .lines .head) .++ (block .lines .tail)
    )

  private def _append (suffix : String) (block : Block) : Block =
    BlockBuilder .mk .build (
      block .lines .:+ (suffix)
    )

  private def _remove_first_line (block : Block) : Block =
    if ( block .lines .isEmpty
    ) block
    else BlockBuilder .mk .build (block .lines .tail)

  private def _translate_block (block : TheoremBlockAnnotation) : TheoremBlockAnnotation =
    TheoremBlockAnnotation_ (
      _append (
        _tc .lean_theorem_end_symbol) (_prepend (
          _tc .lean_theorem_reserved_word) (_remove_first_line (block)
        )
      )
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case TheoremBlockAnnotation_ (block) =>
        _translate_block (TheoremBlockAnnotation_ (block) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanTheoremBlockTranslator_ () extends LeanTheoremBlockTranslator

object LeanTheoremBlockTranslator {
  def mk : LeanTheoremBlockTranslator =
    LeanTheoremBlockTranslator_ ()
}


/**
 * This class translates Soda snippets into Lean snippets.
 */

trait MicroTranslatorToLean
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum
  import   soda.translator.block.BlockAnnotationId
  import   soda.translator.block.BlockTranslatorPipeline
  import   soda.translator.block.ConditionalBlockTranslator
  import   soda.translator.blocktr.TokenReplacement
  import   soda.translator.blocktr.TokenizedBlockTranslator
  import   soda.translator.replacement.Token

  private lazy val _tc = TranslationConstantToLean .mk

  private lazy val _function_definition = BlockAnnotationEnum .mk .function_definition

  private lazy val _class_beginning = BlockAnnotationEnum .mk .class_beginning

  private lazy val _abstract_declaration = BlockAnnotationEnum .mk .abstract_declaration

  private lazy val _class_alias = BlockAnnotationEnum .mk .class_alias

  private lazy val _test_declaration = BlockAnnotationEnum .mk .test_declaration

  lazy val functions_and_tests : Seq [BlockAnnotationId] =
    Seq (_function_definition , _test_declaration)

  lazy val declarations : Seq [BlockAnnotationId] =
    Seq (
      _function_definition , _class_beginning , _abstract_declaration , _class_alias ,
      _test_declaration
    )

  private lazy val _translation_pipeline =
    BlockTranslatorPipeline .mk (
      Seq (
        LeanDocumentationBlockTranslator .mk ,
        LeanDotNotationBlockTranslator .mk ,
        LeanMatchCaseBlockTranslator .mk ,
        LeanClassConstructorBlockTranslator .mk ,
        LeanClassDeclarationBlockTranslator .mk ,
        LeanPackageDeclarationBlockTranslator .mk ,
        LeanClassEndBlockTranslator .mk ,
        LeanClassAliasBlockTranslator .mk ,
        LeanImportDeclarationBlockTranslator .mk ,
        LeanTheoremBlockTranslator .mk ,
        LeanDirectiveBlockTranslator .mk ,
        ConditionalBlockTranslator .mk (functions_and_tests) (
          LeanFunctionDefinitionBlockTranslator .mk) ,
        ConditionalBlockTranslator .mk (functions_and_tests) (
          TokenReplacement .mk .replace_words (_tc .function_symbols_translation) ) ,
        ConditionalBlockTranslator .mk (declarations) (
          TokenReplacement .mk .replace_symbols (_tc .type_symbols_translation) ) ,
        ConditionalBlockTranslator .mk (declarations) (
          TokenReplacement .mk .replace_words (_tc .type_translation) )
      )
    )

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _translation_pipeline .translate (block)

}

case class MicroTranslatorToLean_ () extends MicroTranslatorToLean

object MicroTranslatorToLean {
  def mk : MicroTranslatorToLean =
    MicroTranslatorToLean_ ()
}


/**
 * This class contains constants that are specific for the Soda translator, like reserved words for Soda and Lean.
 */

trait TranslationConstantToLean
{



  import   soda.translator.parser.SodaConstant

  lazy val soda_constant = SodaConstant .mk

  lazy val lean_space = " "

  lazy val lean_new_line = "\n"

  lazy val lean_function_definition_symbol = ":="

  lazy val lean_type_membership_symbol = ":"

  lazy val lean_colon_symbol = ":"

  lazy val lean_subtype_symbol = "<:"

  lazy val lean_supertype_symbol = ">:"

  lazy val lean_function_arrow_symbol = "->"

  lazy val lean_empty_string = ""

  lazy val lean_vertical_bar_symbol = "|"

  lazy val lean_match_end_translation = ""

  lazy val lean_opening_parenthesis = "("

  lazy val lean_closing_parenthesis = ")"

  lazy val lean_comment_opening_symbol = "/-"

  lazy val lean_comment_closing_symbol = "-/"

  lazy val lean_opening_documentation = "/--"

  lazy val lean_closing_documentation = "-/"

  lazy val lean_line_comment = "--"

  lazy val lean_some_variable_name = "x"

  lazy val lean_opening_brace = "{"

  lazy val lean_closing_brace = "}"

  lazy val lean_product_type_symbol = "*"

  lazy val lean_list_constructor_symbol = "::"

  lazy val lean_lambda_arrow_symbol = "=>"

  lazy val lean_case_arrow_symbol = "=>"

  lazy val lean_notation_arrow_symbol = "=>"

  lazy val lean_case_translation = lean_vertical_bar_symbol + lean_space

  lazy val lean_not_reserved_word = "not"

  lazy val lean_and_symbol = "&&"

  lazy val lean_or_symbol = "||"

  lazy val lean_end_symbol = ""

  lazy val lean_dot_notation_symbol = "."

  lazy val lean_comma_symbol = ","

  lazy val lean_quotes_symbol = "\""

  lazy val lean_inductive_end_symbol : String = lean_end_symbol

  lazy val lean_definition_end_symbol : String = lean_end_symbol

  lazy val lean_recursive_definition_end_symbol : String = lean_end_symbol

  lazy val lean_theorem_end_symbol : String = lean_end_symbol

  lazy val lean_default_constructor_name : String = "mk"

  lazy val lean_abbrev_reserved_word : String = "abbrev"

  lazy val lean_add_decl_doc_reserved_word : String = "add_decl_doc"

  lazy val lean_attribute_reserved_word : String = "attribute"

  lazy val lean_admit_reserved_word : String = "admit"

  lazy val lean_axiom_reserved_word : String = "axiom"

  lazy val lean_builtin_initialize_reserved_word : String = "builtin_initialize"

  lazy val lean_by_reserved_word : String = "by"

  lazy val lean_calc_reserved_word : String = "calc"

  lazy val lean_class_reserved_word : String = "class"

  lazy val lean_declare_simp_like_tactic_reserved_word : String = "declare_simp_like_tactic"

  lazy val lean_declare_syntax_cat_reserved_word : String = "declare_syntax_cat"

  lazy val lean_def_reserved_word : String = "def"

  lazy val lean_deriving_reserved_word : String = "deriving"

  lazy val lean_do_reserved_word : String = "do"

  lazy val lean_elab_reserved_word : String = "elab"

  lazy val lean_elab_rules_reserved_word : String = "elab_rules"

  lazy val lean_else_reserved_word : String = "else"

  lazy val lean_end_reserved_word : String = "end"

  lazy val lean_example_reserved_word : String = "example"

  lazy val lean_extends_reserved_word : String = "extends"

  lazy val lean_fun_reserved_word = "fun"

  lazy val lean_if_reserved_word : String = "if"

  lazy val lean_import_reserved_word : String = "import"

  lazy val lean_in_reserved_word : String = "in"

  lazy val lean_inductive_reserved_word : String = "inductive"

  lazy val lean_infix_reserved_word : String = "infix"

  lazy val lean_initialize_reserved_word : String = "initialize"

  lazy val lean_instance_reserved_word : String = "instance"

  lazy val lean_lemma_reserved_word : String = "lemma"

  lazy val lean_let_reserved_word : String = "let"

  lazy val lean_local_reserved_word : String = "local"

  lazy val lean_macro_reserved_word : String = "macro"

  lazy val lean_macro_rules_reserved_word : String = "macro_rules"

  lazy val lean_match_reserved_word : String = "match"

  lazy val lean_namespace_reserved_word : String = "namespace"

  lazy val lean_noncomputable_reserved_word : String = "noncomputable"

  lazy val lean_notation_reserved_word : String = "notation"

  lazy val lean_opaque_reserved_word : String = "opaque"

  lazy val lean_open_reserved_word : String = "open"

  lazy val lean_private_reserved_word : String = "private"

  lazy val lean_rec_reserved_word : String = "rec"

  lazy val lean_scoped_reserved_word : String = "scoped"

  lazy val lean_section_reserved_word : String = "section"

  lazy val lean_set_option_reserved_word : String = "set_option"

  lazy val lean_sorry_reserved_word : String = "sorry"

  lazy val lean_structure_reserved_word : String = "structure"

  lazy val lean_syntax_reserved_word : String = "syntax"

  lazy val lean_then_reserved_word : String = "then"

  lazy val lean_theorem_reserved_word : String = "theorem"

  lazy val lean_unfold_reserved_word : String = "unfold"

  lazy val lean_unif_hint_reserved_word : String = "unif_hint"

  lazy val lean_universe_reserved_word : String = "universe"

  lazy val lean_variable_reserved_word : String = "variable"

  lazy val lean_where_reserved_word : String = "where"

  lazy val lean_with_reserved_word : String = "with"

  lazy val lean_hash_check_reserved_word : String = "#check"

  lazy val lean_hash_eval_reserved_word : String = "#eval"

  lazy val lean_hash_print_reserved_word : String = "#print"

  lazy val lean_hash_reduce_reserved_word : String = "#reduce"

  lazy val lean_all_goals_proof_reserved_word : String = "all_goals"

  lazy val lean_any_goals_proof_reserved_word : String = "any_goals"

  lazy val lean_apply_proof_reserved_word : String = "apply"

  lazy val lean_assumption_proof_reserved_word : String = "assumption"

  lazy val lean_at_proof_reserved_word : String = "at"

  lazy val lean_arg_proof_reserved_word : String = "arg"

  lazy val lean_case_proof_reserved_word : String = "case"

  lazy val lean_cases_proof_reserved_word : String = "cases"

  lazy val lean_constructor_proof_reserved_word : String = "constructor"

  lazy val lean_contradiction_proof_reserved_word : String = "contradiction"

  lazy val lean_conv_proof_reserved_word : String = "conv"

  lazy val lean_congr_proof_reserved_word : String = "congr"

  lazy val lean_exact_proof_reserved_word : String = "exact"

  lazy val lean_exists_proof_reserved_word : String = "exists"

  lazy val lean_first_proof_reserved_word : String = "first"

  lazy val lean_focus_proof_reserved_word : String = "focus"

  lazy val lean_from_proof_reserved_word : String = "from"

  lazy val lean_funext_proof_reserved_word : String = "funext"

  lazy val lean_generalize_proof_reserved_word : String = "generalize"

  lazy val lean_have_proof_reserved_word : String = "have"

  lazy val lean_intro_proof_reserved_word : String = "intro"

  lazy val lean_intros_proof_reserved_word : String = "intros"

  lazy val lean_lhs_proof_reserved_word : String = "lhs"

  lazy val lean_pattern_proof_reserved_word : String = "pattern"

  lazy val lean_propext_proof_reserved_word : String = "propext"

  lazy val lean_rename_i_proof_reserved_word : String = "rename_i"

  lazy val lean_repeat_proof_reserved_word : String = "repeat"

  lazy val lean_revert_proof_reserved_word : String = "revert"

  lazy val lean_rfl_proof_reserved_word : String = "rfl"

  lazy val lean_rhs_proof_reserved_word : String = "rhs"

  lazy val lean_rw_proof_reserved_word : String = "rw"

  lazy val lean_show_proof_reserved_word : String = "show"

  lazy val lean_simp_proof_reserved_word : String = "simp"

  lazy val lean_skip_proof_reserved_word : String = "skip"

  lazy val lean_tactic_proof_reserved_word : String = "tactic"

  lazy val lean_prop_type_name : String = "Prop"

  lazy val lean_set_type_name : String = "Set"

  lazy val lean_type_type_name : String = "Type"

  lazy val lean_decidable_eq_type_name : String = "DecidableEq"

  lazy val lean_recursive_definition_reserved_word : String = lean_def_reserved_word

  lazy val lean_namespace_end_reserved_word : String = lean_end_reserved_word

  lazy val lean_directive_identifier : String = "lean"

  lazy val lean_notation_prefix : String =
    lean_notation_reserved_word + lean_space + lean_quotes_symbol

  lazy val lean_notation_infix : String =
    lean_quotes_symbol + lean_space + lean_notation_arrow_symbol + lean_space

  lazy val lean_main_reserved_words : Seq [String] =
    Seq (
      lean_abbrev_reserved_word ,
      lean_attribute_reserved_word ,
      lean_add_decl_doc_reserved_word ,
      lean_admit_reserved_word ,
      lean_axiom_reserved_word ,
      lean_builtin_initialize_reserved_word ,
      lean_by_reserved_word ,
      lean_calc_reserved_word ,
      lean_class_reserved_word ,
      lean_declare_simp_like_tactic_reserved_word ,
      lean_declare_syntax_cat_reserved_word ,
      lean_def_reserved_word ,
      lean_deriving_reserved_word ,
      lean_do_reserved_word,
      lean_elab_reserved_word ,
      lean_elab_rules_reserved_word ,
      lean_else_reserved_word ,
      lean_end_reserved_word,
      lean_example_reserved_word ,
      lean_fun_reserved_word ,
      lean_if_reserved_word ,
      lean_import_reserved_word ,
      lean_in_reserved_word ,
      lean_inductive_reserved_word ,
      lean_infix_reserved_word ,
      lean_initialize_reserved_word ,
      lean_instance_reserved_word ,
      lean_lemma_reserved_word ,
      lean_let_reserved_word ,
      lean_local_reserved_word ,
      lean_macro_reserved_word ,
      lean_macro_rules_reserved_word ,
      lean_match_reserved_word  ,
      lean_namespace_reserved_word ,
      lean_noncomputable_reserved_word ,
      lean_notation_reserved_word ,
      lean_opaque_reserved_word ,
      lean_open_reserved_word ,
      lean_private_reserved_word ,
      lean_rec_reserved_word ,
      lean_scoped_reserved_word ,
      lean_section_reserved_word ,
      lean_set_option_reserved_word ,
      lean_sorry_reserved_word ,
      lean_structure_reserved_word ,
      lean_syntax_reserved_word ,
      lean_then_reserved_word ,
      lean_theorem_reserved_word ,
      lean_unfold_reserved_word ,
      lean_unif_hint_reserved_word ,
      lean_universe_reserved_word ,
      lean_variable_reserved_word ,
      lean_where_reserved_word ,
      lean_with_reserved_word
    )

  lazy val lean_hash_reserved_words : Seq [String] =
    Seq (
      lean_hash_check_reserved_word ,
      lean_hash_eval_reserved_word ,
      lean_hash_print_reserved_word ,
      lean_hash_reduce_reserved_word
    )

  lazy val lean_proof_reserved_words : Seq [String] =
    Seq (
      lean_all_goals_proof_reserved_word ,
      lean_any_goals_proof_reserved_word ,
      lean_apply_proof_reserved_word ,
      lean_arg_proof_reserved_word ,
      lean_assumption_proof_reserved_word ,
      lean_at_proof_reserved_word ,
      lean_case_proof_reserved_word ,
      lean_cases_proof_reserved_word ,
      lean_constructor_proof_reserved_word ,
      lean_contradiction_proof_reserved_word ,
      lean_conv_proof_reserved_word ,
      lean_congr_proof_reserved_word ,
      lean_exact_proof_reserved_word ,
      lean_exists_proof_reserved_word ,
      lean_first_proof_reserved_word ,
      lean_focus_proof_reserved_word ,
      lean_from_proof_reserved_word ,
      lean_funext_proof_reserved_word ,
      lean_generalize_proof_reserved_word ,
      lean_have_proof_reserved_word ,
      lean_intro_proof_reserved_word ,
      lean_intros_proof_reserved_word ,
      lean_lhs_proof_reserved_word ,
      lean_pattern_proof_reserved_word ,
      lean_propext_proof_reserved_word ,
      lean_rename_i_proof_reserved_word ,
      lean_repeat_proof_reserved_word ,
      lean_revert_proof_reserved_word ,
      lean_rfl_proof_reserved_word ,
      lean_rhs_proof_reserved_word ,
      lean_rw_proof_reserved_word ,
      lean_show_proof_reserved_word ,
      lean_simp_proof_reserved_word ,
      lean_skip_proof_reserved_word ,
      lean_tactic_proof_reserved_word
    )

  lazy val lean_type_reserved_words : Seq [String] =
    Seq (
      lean_prop_type_name ,
      lean_set_type_name ,
      lean_type_type_name ,
      lean_decidable_eq_type_name
    )

  lazy val lean_reserved_words : Seq [String] =
    lean_main_reserved_words .++ (
    lean_hash_reserved_words .++ (
    lean_proof_reserved_words .++ (
    lean_type_reserved_words) ) )

  lazy val lean_recursive_function_prefixes : Seq [String] =
    Seq (
      "rec_",
      "_rec_",
      "tailrec_",
      "_tailrec_",
      "@tailrec"
    )

  lazy val non_definition_block_prefixes : Seq [String] =
    Seq (
      soda_constant .package_reserved_word ,
      soda_constant .import_reserved_word ,
      soda_constant .class_end_reserved_word ,
      soda_constant .class_reserved_word ,
      soda_constant .comment_opening_symbol
    )

  lazy val type_symbols_translation : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (soda_constant .subtype_reserved_word , lean_subtype_symbol) ,
      Tuple2 (soda_constant .supertype_reserved_word , lean_supertype_symbol) ,
      Tuple2 (soda_constant .function_arrow_symbol , lean_function_arrow_symbol) ,
      Tuple2 (soda_constant .opening_bracket_symbol, lean_opening_parenthesis + lean_space) ,
      Tuple2 (soda_constant .closing_bracket_symbol , lean_space + lean_closing_parenthesis)
    )

  lazy val function_symbols_translation : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (soda_constant .function_definition_symbol , lean_function_definition_symbol) ,
      Tuple2 (soda_constant .lambda_reserved_word , lean_fun_reserved_word) ,
      Tuple2 (soda_constant .any_reserved_word , lean_fun_reserved_word) ,
      Tuple2 (soda_constant .lambda_arrow_symbol , lean_lambda_arrow_symbol) ,
      Tuple2 (soda_constant .case_arrow_symbol , lean_case_arrow_symbol) ,
      Tuple2 (soda_constant .not_reserved_word , lean_not_reserved_word) ,
      Tuple2 (soda_constant .and_reserved_word , lean_and_symbol) ,
      Tuple2 (soda_constant .or_reserved_word , lean_or_symbol) ,
      Tuple2 (soda_constant .tail_recursion_annotation , lean_empty_string) ,
      Tuple2 (soda_constant .override_annotation , lean_empty_string) ,
      Tuple2 (soda_constant .new_annotation , lean_empty_string) ,
      Tuple2 (soda_constant .seq_constructor_symbol , lean_list_constructor_symbol)
    )

  lazy val type_translation : Seq [Tuple2 [String, String]  ] =
    Seq (
        Tuple2 ("Boolean" , "Bool"),
        Tuple2 ("None" , "Option.none"),
        Tuple2 ("Some" , "Option.some"),
        Tuple2 ("Seq" , "List"),
        Tuple2 ("Nil" , "List.nil"),
        Tuple2 ("Tuple2" , "Prod")
    )

  lazy val lean_prelude : Seq [String] =
    type_translation
      .map (
         pair =>
          lean_notation_prefix + pair ._1 + lean_notation_infix + pair ._2
      )

  lazy val prefix_lean_non_soda : String = "__soda__"

  lazy val lean_non_soda : Seq [Tuple2 [String, String] ] =
    lean_reserved_words
      .filter ( x => ! soda_constant .soda_reserved_words  .contains (x))
      .map ( x => Tuple2 (x , prefix_lean_non_soda + x) )

  def is_lean_word (word : String) : Boolean =
    lean_reserved_words .contains (word)

  def is_soda_word (word : String) : Boolean =
    soda_constant .soda_reserved_words .contains (word)

}

case class TranslationConstantToLean_ () extends TranslationConstantToLean

object TranslationConstantToLean {
  def mk : TranslationConstantToLean =
    TranslationConstantToLean_ ()
}


trait FileNamePair
{

  def   input_file_name : String
  def   output_file_name : String

}

case class FileNamePair_ (input_file_name : String, output_file_name : String) extends FileNamePair

object FileNamePair {
  def mk (input_file_name : String) (output_file_name : String) : FileNamePair =
    FileNamePair_ (input_file_name, output_file_name)
}

/**
 * This translates Soda source code to Lean source code.
 */

trait TranslatorToLean
  extends
    soda.translator.extension.common.Extension
{



  import   soda.translator.block.DefaultBlockSequenceTranslator
  import   soda.translator.io.DirectoryProcessor
  import   soda.translator.io.SimpleFileReader
  import   soda.translator.io.SimpleFileWriter
  import   soda.translator.parser.BlockProcessor
  import   java.io.File

  private lazy val _soda_extension : String = ".soda"

  private lazy val _lean_extension : String = ".lean"

  private lazy val _default_argument = "."

  private lazy val _translator =
    BlockProcessor .mk (
      DefaultBlockSequenceTranslator .mk (
        MicroTranslatorToLean .mk
      )
    )

  private def _process_soda_file_with (pair : FileNamePair) : Boolean =
    _translate (pair .input_file_name) (pair .output_file_name)

  private def _get_input_output_file_names (input_name : String) : FileNamePair =
    if ( input_name .endsWith (_soda_extension)
    ) FileNamePair .mk (input_name) (
      input_name .substring (0, input_name .length - _soda_extension .length) + _lean_extension)
    else FileNamePair .mk (input_name + _soda_extension) (input_name + _lean_extension)

  private def _process_soda_file (file : File) : Boolean =
    _process_soda_file_with (_get_input_output_file_names (file .getAbsolutePath) )

  private def _process_directory (start : String) : Boolean =
    DirectoryProcessor .mk (start) (_process_soda_file) .process ()

  private def _translate_with_input (input : String) (output_file_name : String) : Boolean =
    SimpleFileWriter .mk .write_file (
      output_file_name) (
      content = _translator .translate (input)
    )

  private def _translate (input_file_name : String) (output_file_name : String) : Boolean =
    _translate_with_input (
      SimpleFileReader .mk .read_file (input_file_name) ) (
      output_file_name
    )

  def execute_for (arguments : Seq [String] ) : Boolean =
    arguments.length match  {
      case 0 => _process_directory (_default_argument)
      case 1 => _process_directory (arguments (0) )
      case 2 => _translate (arguments (0) ) (arguments (1) )
      case _otherwise => false
    }

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      execute_for (arguments)

}

case class TranslatorToLean_ () extends TranslatorToLean

object TranslatorToLean {
  def mk : TranslatorToLean =
    TranslatorToLean_ ()
}

