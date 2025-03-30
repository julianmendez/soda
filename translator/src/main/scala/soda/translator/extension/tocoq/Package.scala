package soda.translator.extension.tocoq

/*
 * This package contains classes for the translation to Gallina,
 * the specification language used by Coq.
 */







trait CoqClassAliasBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.SodaConstant
  import   soda.translator.parser.annotation.ClassAliasAnnotation
  import   soda.translator.parser.annotation.ClassAliasAnnotation_

  private lazy val _sc : SodaConstant = SodaConstant .mk

  private lazy val _tc : TranslationConstantToCoq = TranslationConstantToCoq .mk

  def get_first_line (block : Block) : String =
    block .lines .headOption .getOrElse ("")

  private def _process_class_alias (line : String) : String =
    line
      .replace (_sc .class_alias_reserved_word + _sc .space , _tc .coq_notation_prefix)
      .replace (_sc .space + _sc .class_alias_definition_symbol + _sc .space ,
         _tc .coq_notation_infix) +
      _tc .coq_notation_suffix

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

case class CoqClassAliasBlockTranslator_ () extends CoqClassAliasBlockTranslator

object CoqClassAliasBlockTranslator {
  def mk : CoqClassAliasBlockTranslator =
    CoqClassAliasBlockTranslator_ ()
}


trait CoqClassConstructorBlockTranslator
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

  private lazy val _tc : TranslationConstantToCoq = TranslationConstantToCoq .mk

  private lazy val _sp : String = _tc .coq_space

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
      .replaceAll (_sc .subtype_reserved_word , _tc .coq_subtype_symbol)
      .replaceAll (_sc .supertype_reserved_word , _tc .coq_supertype_symbol)
      .replaceAll (_sc .function_arrow_symbol , _tc .coq_function_arrow_symbol)

  private def _get_types_of_abstract_functions (block : AbstractDeclarationAnnotation) : Seq [String] =
    block .abstract_functions
      .map ( annotated_line => _translate_type_symbols (annotated_line .line) .trim)

  private def _get_first_line (block : AnnotatedBlock) : String =
    block .lines .headOption .getOrElse ("")

  private def _get_initial_spaces_with (line : String) : String =
    line .takeWhile ( ch => ch .isSpaceChar)

  private def _get_initial_spaces (block : AnnotatedBlock) : String =
    _get_initial_spaces_with (_get_first_line (block) )

  private lazy val _two_spaces : String = _sp + _sp

  private lazy val _four_spaces : String = _two_spaces + _two_spaces

  private def _get_constructor_declaration (beginning : ClassBeginningAnnotation)
      (functions : Seq [String] ) : String =
    _get_initial_spaces (beginning) + _two_spaces +
    _sc .default_constructor_function +
    _sp +
    _tc .coq_opening_brace +
    _tc .coq_new_line +
    _get_initial_spaces (beginning) + _four_spaces +
    functions .mkString (_sp + _tc .coq_semicolon_symbol + _tc .coq_new_line +
      _get_initial_spaces (beginning) + _four_spaces) +
    _tc .coq_new_line +
    _tc .coq_closing_brace + _sp + _tc .coq_end_symbol + _tc .coq_new_line +
    _tc .coq_new_line +
    _tc .coq_notation_prefix +
    beginning .class_name + _sc .constructor_suffix +
    _tc .coq_notation_infix +
    beginning .class_name + _tc .coq_dot_notation_symbol +
    _sc .default_constructor_function + _tc .coq_notation_suffix

  private def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation)
      (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    AbstractDeclarationAnnotation .mk (
      BlockBuilder .mk .build (
        Seq [String] (
          _get_constructor_declaration (beginning) (_get_types_of_abstract_functions (block) ) )
      )
    ) (
      block .references
    )

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

case class CoqClassConstructorBlockTranslator_ () extends CoqClassConstructorBlockTranslator

object CoqClassConstructorBlockTranslator {
  def mk : CoqClassConstructorBlockTranslator =
    CoqClassConstructorBlockTranslator_ ()
}


trait CoqClassDeclarationBlockTranslator
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

  private lazy val _tc = TranslationConstantToCoq .mk

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
    ) lines .map ( line =>
       _tc .coq_import_reserved_word + _tc .coq_space + line .trim +
       _tc .coq_space + _tc .coq_end_symbol)
    else Seq [String] ()

  def remove_first_line (lines : Seq [String] ) : Seq [String] =
    if ( lines .isEmpty
    ) lines
    else lines .tail

  def remove_first_line (block : Block) : Block =
    BlockBuilder .mk .build (remove_first_line (block .lines) )

  private def _process_if_extends (lines : Seq [String] ) : Seq [String] =
    if ( (get_first_line (lines) .trim == _sc .extends_reserved_word)
    ) Seq [String] (
      get_initial_spaces (lines) ) .++ (_process_after_extends (remove_first_line (lines) ) )
    else lines

  private def _process_tail (lines : Seq [String] ) : Seq [String] =
    _process_if_extends (remove_first_line (lines) )

  lazy val table_translator : Translator =
    TableTranslator_ (
      Seq (Tuple2 (_sc .class_reserved_word, _tc .coq_class_reserved_word ) )
    )

  private def _process_head_with (class_name : String) (line : String) : Seq [String] =
    Seq [String] (
      _tc .coq_module_reserved_word + _tc .coq_space + class_name + _tc .coq_space +
      _tc .coq_end_symbol + _tc .coq_new_line + _tc .coq_new_line +
      Replacement_ (_sc .space + line)
        .replace_at_beginning (0) (table_translator)
        .line .substring (_sc .space .length) +
      _tc .coq_space + _tc .coq_type_membership_symbol + _tc .coq_space +
      _tc .coq_type_reserved_word + _tc .coq_space + _tc .coq_function_definition_symbol
    )

  private def _process_head (class_name : String) (lines : Seq [String] ) : Seq [String] =
    _process_head_with (class_name) (get_first_line (lines) )

  def contains_equals (line : String) : Boolean =
    line .trim .contains (_sc .function_definition_symbol)

  def has_condition_for_type_alias (line : String) : Boolean =
    contains_equals (line)

  private def _translate_block (block : ClassBeginningAnnotation) : Block =
    if ( (has_condition_for_type_alias (get_first_line (block .lines) ) )
    ) block
    else
      BlockBuilder .mk .build (
        _process_head (block .class_name) (block .lines) ++ _process_tail (block .lines)
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

case class CoqClassDeclarationBlockTranslator_ () extends CoqClassDeclarationBlockTranslator

object CoqClassDeclarationBlockTranslator {
  def mk : CoqClassDeclarationBlockTranslator =
    CoqClassDeclarationBlockTranslator_ ()
}


trait CoqClassEndBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  private lazy val _tc = TranslationConstantToCoq .mk

  private def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation)
      (block : ClassEndAnnotation) : ClassEndAnnotation =
    ClassEndAnnotation .mk (
      BlockBuilder .mk .build (
        Seq [String] (
          _tc .coq_module_end_reserved_word + _tc .coq_space + beginning .class_name +
          _tc .coq_space + _tc .coq_end_symbol ,
          "",
          _tc .coq_import_reserved_word + _tc .coq_space + beginning .class_name +
          _tc .coq_space + _tc .coq_end_symbol
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
        _translate_block (ClassEndAnnotation .mk (block) (references) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqClassEndBlockTranslator_ () extends CoqClassEndBlockTranslator

object CoqClassEndBlockTranslator {
  def mk : CoqClassEndBlockTranslator =
    CoqClassEndBlockTranslator_ ()
}


trait CoqDatatypeDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.Block
  import   soda.translator.parser.SodaConstant
  import   soda.translator.parser.annotation.DatatypeDeclarationAnnotation
  import   soda.translator.parser.annotation.DatatypeDeclarationAnnotation_

  private lazy val _sc = SodaConstant .mk

  private lazy val _tc = TranslationConstantToCoq .mk

  private def _add_final_dot (lines : Seq [AnnotatedLine] ) : Seq [AnnotatedLine] =
    lines .++ (Seq [AnnotatedLine] () .+: (
        AnnotatedLine .mk (_tc .coq_dot_notation_symbol) (false) ) )

  private def _remove_variables_with (parameter : String) (index : Int) : String =
    if ( index >= 0 &&
      parameter .startsWith (_sc .opening_parenthesis_symbol) &&
      parameter .endsWith (_sc .closing_parenthesis_symbol)
    ) _tc .coq_opening_parenthesis +
       parameter
         .substring (index + _sc .type_membership_symbol .length)
         .trim
    else parameter

  private def _remove_variables (parameter : String) : String =
    _remove_variables_with (parameter) (parameter .indexOf (_sc .type_membership_symbol) )

  private def _parameters_without_variables (parameters : Seq [String] ) : Seq [String] =
    parameters
      .map ( parameter => _remove_variables (parameter .trim) )

  private def _get_type_parameters_with (parameters : Seq [String] ) : String =
    if ( parameters .isEmpty
    ) ""
    else
      parameters
        .map ( param => _tc .coq_space + _tc .coq_opening_parenthesis +
          param + _tc .coq_space + _tc .coq_type_membership_symbol +
          _tc .coq_space + _tc .coq_type_reserved_word  + _tc .coq_closing_parenthesis)
        .mkString

  def get_type_parameters (block : DatatypeDeclarationAnnotation) : String =
    _get_type_parameters_with (block .type_parameters)

  private def _translate_parameter (parameter : String) : String =
    parameter
      .replace (_sc .opening_bracket_symbol , _tc .coq_opening_parenthesis)
      .replace (_sc .closing_bracket_symbol , _tc .coq_closing_parenthesis)

  private def _translate_constructors (block : DatatypeDeclarationAnnotation)
      : Seq [AnnotatedLine] =
    block .constructors
      .map ( constr =>
          _tc .coq_space + _tc .coq_space + _tc .coq_vertical_bar_symbol + _tc .coq_space +
          constr .name +  _tc .coq_space + _tc .coq_type_membership_symbol + _tc .coq_space +
          _parameters_without_variables (constr .parameters)
            .map ( param => _translate_parameter (param) )
            .mkString (_tc .coq_space + _tc .coq_function_arrow_symbol + _tc .coq_space) )
      .map ( line => AnnotatedLine .mk (line) (false) )

  def get_number_of_spaces_at_beginning (line : String) : Int =
    line
      .takeWhile ( ch => ch .isSpaceChar)
      .length

  def get_first_line (block : AnnotatedBlock) : String =
    block .lines .headOption .getOrElse ("")

  def prepend_aligned_non_comment (index : Int) (prefix : String) (annotated_line : AnnotatedLine)
      : AnnotatedLine =
    if ( annotated_line .is_comment
    ) annotated_line
    else AnnotatedLine .mk (annotated_line .line .substring (0 , index) + prefix +
      annotated_line .line .substring (index) ) (annotated_line .is_comment)

  def prepend_to_lines_aligned_at (number_of_spaces : Int) (prefix : String)
      (annotated_lines : Seq [AnnotatedLine] ) : Seq [AnnotatedLine] =
    annotated_lines .map ( annotated_line =>
      prepend_aligned_non_comment (number_of_spaces) (prefix) (annotated_line) )

  def get_type_declaration (block : DatatypeDeclarationAnnotation) : AnnotatedLine =
    AnnotatedLine .mk (
      _tc .coq_inductive_reserved_word + _tc .coq_space +
      block .class_name + get_type_parameters (block) + _tc .coq_space +
      _tc .coq_type_membership_symbol + _tc .coq_space +
      _tc .coq_type_reserved_word + _tc .coq_space +
      _tc .coq_function_definition_symbol
    ) (false)

  private def _translate_block (block : DatatypeDeclarationAnnotation) : DatatypeDeclarationAnnotation =
    DatatypeDeclarationAnnotation .mk (
      Block .mk (
        (Seq [AnnotatedLine] () .+: (get_type_declaration (block) ) ) .++ (
          _add_final_dot (_translate_constructors (block) ) )
      )
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case DatatypeDeclarationAnnotation_ (block) =>
        _translate_block (DatatypeDeclarationAnnotation .mk (block) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqDatatypeDeclarationBlockTranslator_ () extends CoqDatatypeDeclarationBlockTranslator

object CoqDatatypeDeclarationBlockTranslator {
  def mk : CoqDatatypeDeclarationBlockTranslator =
    CoqDatatypeDeclarationBlockTranslator_ ()
}


trait CoqDirectiveBlockTranslator
  extends
    soda.translator.blocktr.DirectiveBlockTranslator
{



  private lazy val _tc = TranslationConstantToCoq .mk

  lazy val identifier : String = _tc .coq_directive_identifier

  lazy val opening_comment : String = _tc .coq_comment_opening_symbol

  lazy val closing_comment : String = _tc .coq_comment_closing_symbol

}

case class CoqDirectiveBlockTranslator_ () extends CoqDirectiveBlockTranslator

object CoqDirectiveBlockTranslator {
  def mk : CoqDirectiveBlockTranslator =
    CoqDirectiveBlockTranslator_ ()
}


trait CoqDocumentationBlockTranslator
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

  private lazy val _tc = TranslationConstantToCoq .mk

  private def _prepend (prefix : String) (content : Seq [String] ) : Seq [String] =
    if ( content .isEmpty
    ) Seq [String] (prefix)
    else Seq [String] (prefix + content .head) .++ (content .tail)

  private def _append (suffix : String) (content : Seq [String] ) : Seq [String] =
    content .:+ (suffix)

  private def _translate_with_symbol (opening_symbol : String) (lines : Seq [String] ) : Seq [String] =
    _append (
      _tc .coq_comment_closing_symbol) (
      _prepend (opening_symbol) (
        CommentDelimiterRemover .mk
          .remove_comment_delimiters (lines)
      )
    )

  private def _get_opening_symbol (line : String) : String =
    if ( (line .trim .startsWith (_sc .documentation_comment_opening_symbol) )
    ) _tc .coq_opening_documentation
    else _tc .coq_comment_opening_symbol

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

case class CoqDocumentationBlockTranslator_ () extends CoqDocumentationBlockTranslator

object CoqDocumentationBlockTranslator {
  def mk : CoqDocumentationBlockTranslator =
    CoqDocumentationBlockTranslator_ ()
}


trait CoqDotNotationBlockTranslator
  extends
    soda.translator.blocktr.TokenizedBlockTranslator
{



  import   soda.translator.parser.SodaConstant
  import   soda.translator.replacement.Token

  private lazy val _sc = SodaConstant .mk

  private lazy val _tc = TranslationConstantToCoq .mk

  lazy val fold_while = soda.lib.FoldWhile .mk

  private def _applies_dot_notation (word : String) : Boolean =
    word .startsWith (_sc .dot_notation_symbol) &&
      word .length > _sc .dot_notation_symbol .length

  private def _translate_dot_notation (word : String) : String =
    _tc .coq_dot_notation_symbol +
    _tc .coq_opening_parenthesis +
    word .substring (_sc .dot_notation_symbol .length) +
    _tc .coq_closing_parenthesis

  private def _add_parentheses_if_necessary (word : String) : String =
    if ( _applies_dot_notation (word)
    ) _translate_dot_notation (word)
    else word

  private def _add_parentheses_to_dotted_words (line : String) : String =
    line
      .split (_sc .space)
      .map ( word => _add_parentheses_if_necessary (word) )
      .mkString (_sc .space)

  private def _add_space_if_necessary (original : String ) (text : String) : String =
    if ( (original .endsWith (_sc .space) )
    ) text + _sc .space
    else text

  lazy val replace_token : Token => String =
     token =>
      _add_space_if_necessary (token .text) (
        _add_parentheses_to_dotted_words (token .text)
      )

}

case class CoqDotNotationBlockTranslator_ () extends CoqDotNotationBlockTranslator

object CoqDotNotationBlockTranslator {
  def mk : CoqDotNotationBlockTranslator =
    CoqDotNotationBlockTranslator_ ()
}


trait CoqFunctionDefinitionBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.SodaConstant
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_
  import   soda.translator.parser.tool.CommentDelimiterRemover

  private lazy val _tc = TranslationConstantToCoq .mk

  private lazy val _sc = SodaConstant .mk

  private lazy val _cc = CommentDelimiterRemover .mk

  private lazy val _soda_def_prefix = _sc .def_reserved_word + _sc .space

  private lazy val _empty_string = ""

  private def _append (suffix : String) (block : Block) : Block =
    BlockBuilder .mk .build (
      block .lines .:+ (suffix)
    )

  private def _prepend (prefix : String) (block : Block) : Block =
    BlockBuilder .mk .build (
      Seq [String] (prefix + block .lines .head) .++ (block .lines .tail)
    )

  def is_a_definition (block : Block) : Boolean =
    ! is_a_recursive_definition (block) &&
    ! _tc .non_definition_block_prefixes .exists (
       prefix => block .contents .trim .startsWith (prefix)
    )

  private def _translate_non_recursive_definition (block : FunctionDefinitionAnnotation) : Block =
    if ( is_a_definition (block)
    ) _append (_tc .coq_definition_end_symbol) (_prepend (_tc .coq_definition_reserved_word +
       _tc .coq_space) (block) )
    else block

  def first_line (block : Block) : String =
    block .lines .headOption .getOrElse (_empty_string) .trim

  def is_a_recursive_definition (block : Block) : Boolean =
    _tc .coq_recursive_function_prefixes .exists (
       prefix => first_line (block) .startsWith (prefix)
    )

  private def _remove_def_if_present (block : FunctionDefinitionAnnotation) : FunctionDefinitionAnnotation =
    if ( (block .lines .nonEmpty) &&
      (block .lines .head .trim .startsWith (_soda_def_prefix) )
    )
      FunctionDefinitionAnnotation .mk (
        BlockBuilder .mk .build (
        (Seq [String] ()
          .+: (_cc .remove_part (block .lines .head) (_soda_def_prefix) ) )
          .++ (block .lines .tail)
        )
      )
    else block

  private def _translate_block_with (block : FunctionDefinitionAnnotation) : Block =
    if ( is_a_recursive_definition (block)
    ) _append (_tc .coq_recursive_definition_end_symbol) (
      _prepend (_tc .coq_recursive_definition_reserved_word + _tc .coq_space) (block) )
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

case class CoqFunctionDefinitionBlockTranslator_ () extends CoqFunctionDefinitionBlockTranslator

object CoqFunctionDefinitionBlockTranslator {
  def mk : CoqFunctionDefinitionBlockTranslator =
    CoqFunctionDefinitionBlockTranslator_ ()
}


trait CoqImportDeclarationBlockTranslator
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

  private lazy val _tc = TranslationConstantToCoq .mk

  lazy val coq_import_declaration_pattern =
    _tc .coq_import_reserved_word + _tc .coq_space

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
        coq_import_declaration_pattern) (
        block .imported_items
          .filter( annotated_line => ! annotated_line .is_comment)
          .map ( annotated_line => AnnotatedLine_ (annotated_line .line +
             _tc .coq_space + _tc .coq_end_symbol, false)
          )
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

case class CoqImportDeclarationBlockTranslator_ () extends CoqImportDeclarationBlockTranslator

object CoqImportDeclarationBlockTranslator {
  def mk : CoqImportDeclarationBlockTranslator =
    CoqImportDeclarationBlockTranslator_ ()
}


trait CoqMatchCaseBlockTranslator
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

  private lazy val _tc = TranslationConstantToCoq .mk

  private lazy val _soda_match_pattern = _sc .match_reserved_word + " "

  private def _is_a_match_line (line : String) : Boolean =
    line .trim .startsWith (_soda_match_pattern)

  private def _is_a_match_case_structure (block : AnnotatedBlock) : Boolean =
    block .lines .exists ( line => _is_a_match_line (line) )

  private def _append_with_after_match (line : String) : String =
    if ( _is_a_match_line (line)
    ) line + _tc .coq_space + _tc .coq_with_reserved_word
    else line

  private def _is_a_case_line (line : String) : Boolean =
    line .trim .startsWith (_soda_case_pattern)

  private def _replace_case (line : String) : String =
    if ( _is_a_case_line (line)
    )
      ReplacementAux .mk
        . replace_first (line) (_soda_case_pattern) (_tc .coq_case_translation)
    else line

  private def _left_part (index : Int) (line : String) : String =
    line .substring (0 , index)

  private def _get_tabulation_of_match (block : AnnotatedBlock) : String =
    block .lines
      .find ( line => _is_a_match_line (line) )
      .map ( line => _left_part (line .indexOf (_soda_match_pattern) ) (line) )
      .getOrElse (_tc .coq_space)

  private def _translate_match_case_structure (block: AnnotatedBlock) (tabulation : String) : Block =
    BlockBuilder .mk .build (
      block .lines
        .map ( line => _append_with_after_match (line) )
        .map ( line => _replace_case (line) )
        .++ (Seq [String] () .+: (tabulation + _tc .coq_match_end_translation) )
    )

  private def _translate_block (block : AnnotatedBlock) : Block =
    if ( _is_a_match_case_structure (block)
    ) _translate_match_case_structure (block) (_get_tabulation_of_match (block) )
    else block

  private def _translate_function_block (block : AnnotatedBlock) : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation_ (_translate_block (block) )

  private def _translate_test_block (block : AnnotatedBlock) : TestDeclarationAnnotation =
    TestDeclarationAnnotation_ (_translate_block (block) )

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

case class CoqMatchCaseBlockTranslator_ () extends CoqMatchCaseBlockTranslator

object CoqMatchCaseBlockTranslator {
  def mk : CoqMatchCaseBlockTranslator =
    CoqMatchCaseBlockTranslator_ ()
}


trait CoqPackageDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.PackageDeclarationAnnotation
  import   soda.translator.parser.annotation.PackageDeclarationAnnotation_

  private lazy val _tc = TranslationConstantToCoq .mk

  private def _comment_block (block : Block) : Block =
    BlockBuilder .mk .build (
      ( (Seq (_tc .coq_comment_opening_symbol) .++ (block .lines) ) .++ (
        Seq (_tc .coq_comment_closing_symbol) ) ) .++ (_tc .coq_prelude)
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

case class CoqPackageDeclarationBlockTranslator_ () extends CoqPackageDeclarationBlockTranslator

object CoqPackageDeclarationBlockTranslator {
  def mk : CoqPackageDeclarationBlockTranslator =
    CoqPackageDeclarationBlockTranslator_ ()
}


trait CoqTheoremBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder
  import   soda.translator.parser.annotation.TheoremBlockAnnotation
  import   soda.translator.parser.annotation.TheoremBlockAnnotation_

  private lazy val _tc = TranslationConstantToCoq .mk

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
    TheoremBlockAnnotation .mk (
      _append (
        _tc .coq_theorem_end_symbol) (_prepend (
          _tc .coq_theorem_begin_reserved_word) (_remove_first_line (block)
        )
      )
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case TheoremBlockAnnotation_ (block) =>
        _translate_block (TheoremBlockAnnotation .mk (block) )
      case _otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqTheoremBlockTranslator_ () extends CoqTheoremBlockTranslator

object CoqTheoremBlockTranslator {
  def mk : CoqTheoremBlockTranslator =
    CoqTheoremBlockTranslator_ ()
}


/**
 * This class translates Soda snippets into Coq snippets.
 */

trait MicroTranslatorToCoq
  extends
    soda.translator.block.BlockTranslator
{



  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.BlockAnnotationId
  import   soda.translator.block.BlockTranslatorPipeline
  import   soda.translator.block.ConditionalBlockTranslator
  import   soda.translator.blocktr.TokenReplacement

  private lazy val _tc = TranslationConstantToCoq .mk

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
        CoqDocumentationBlockTranslator .mk ,
        CoqDotNotationBlockTranslator .mk ,
        CoqMatchCaseBlockTranslator .mk ,
        CoqClassConstructorBlockTranslator .mk ,
        CoqClassDeclarationBlockTranslator .mk ,
        CoqPackageDeclarationBlockTranslator .mk ,
        CoqClassEndBlockTranslator .mk ,
        CoqClassAliasBlockTranslator .mk ,
        CoqImportDeclarationBlockTranslator .mk ,
        CoqDatatypeDeclarationBlockTranslator .mk ,
        CoqTheoremBlockTranslator .mk ,
        CoqDirectiveBlockTranslator .mk ,
        ConditionalBlockTranslator .mk (functions_and_tests) (
          CoqFunctionDefinitionBlockTranslator .mk) ,
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

case class MicroTranslatorToCoq_ () extends MicroTranslatorToCoq

object MicroTranslatorToCoq {
  def mk : MicroTranslatorToCoq =
    MicroTranslatorToCoq_ ()
}


/**
 * This class contains constants that are specific for the Soda translator,
 * like reserved words for Soda and Coq.
 */

trait TranslationConstantToCoq
{



  import   soda.translator.parser.SodaConstant

  lazy val soda_constant = SodaConstant .mk

  lazy val coq_space = " "

  lazy val coq_new_line = "\n"

  lazy val coq_function_definition_symbol = ":="

  lazy val coq_type_membership_symbol = ":"

  lazy val coq_subtype_symbol = "<:"

  lazy val coq_supertype_symbol = ">:"

  lazy val coq_function_arrow_symbol = "->"

  lazy val coq_empty_string = ""

  lazy val coq_vertical_bar_symbol = "|"

  lazy val coq_match_end_translation = "end"

  lazy val coq_opening_parenthesis = "("

  lazy val coq_closing_parenthesis = ")"

  lazy val coq_comment_opening_symbol = "(*"

  lazy val coq_comment_closing_symbol = "*)"

  lazy val coq_opening_documentation = "(**"

  lazy val coq_closing_documentation = "*)"

  lazy val coq_some_variable_name = "x"

  lazy val coq_opening_brace = "{"

  lazy val coq_closing_brace = "}"

  lazy val coq_semicolon_symbol = ";"

  lazy val coq_dot_notation_symbol = "."

  lazy val coq_product_type_symbol = "*"

  lazy val coq_lambda_reserved_word = "fun"

  lazy val coq_lambda_arrow_symbol = "=>"

  lazy val coq_case_arrow_symbol = "=>"

  lazy val coq_quotes_symbol = "\""

  lazy val coq_apostrophe_symbol = "'"

  lazy val coq_equals_symbol = "=?"

  lazy val coq_less_than_symbol = "<?"

  lazy val coq_less_than_or_equal_to_symbol = "<=?"

  lazy val coq_case_translation = coq_vertical_bar_symbol + coq_space

  lazy val coq_not_reserved_word = "negb"

  lazy val coq_and_reserved_word = "andb"

  lazy val coq_or_reserved_word = "orb"

  lazy val coq_end_symbol = "."

  lazy val coq_class_reserved_word : String = "Class"

  lazy val coq_definition_reserved_word : String = "Definition"

  lazy val coq_inductive_reserved_word : String = "Inductive"

  lazy val coq_set_reserved_word : String = "Set"

  lazy val coq_type_reserved_word : String = "Type"

  lazy val coq_module_reserved_word : String = "Module"

  lazy val coq_module_end_reserved_word : String = "End"

  lazy val coq_import_reserved_word : String = "Import"

  lazy val coq_notation_reserved_word : String = "Notation"

  lazy val coq_recursive_definition_reserved_word : String = "Fixpoint"

  lazy val coq_inductive_end_symbol : String = coq_end_symbol

  lazy val coq_definition_end_symbol : String = coq_end_symbol

  lazy val coq_recursive_definition_end_symbol : String = coq_end_symbol

  lazy val coq_with_reserved_word : String = "with"

  lazy val coq_theorem_begin_reserved_word : String = "Theorem"

  lazy val coq_theorem_end_symbol : String = coq_end_symbol

  lazy val coq_directive_identifier : String = "coq"

  lazy val coq_notation_at_level_99 : String = "(at level 99)"

  lazy val coq_notation_prefix : String =
    coq_notation_reserved_word + coq_space + coq_quotes_symbol + coq_apostrophe_symbol

  lazy val coq_notation_infix : String =
     coq_apostrophe_symbol + coq_quotes_symbol + coq_space + coq_function_definition_symbol +
     coq_space

  lazy val coq_notation_suffix : String =
    coq_space + coq_notation_at_level_99 + coq_space + coq_end_symbol

  lazy val coq_prelude : Seq [String] =
    Seq (
      "",
      "Require Import Coq.ZArith.BinInt .",
      "(* https://coq.inria.fr/library/Coq.ZArith.BinInt.html *)",
      "",
      "Require Import Coq.Lists.List .",
      "(* https://coq.inria.fr/library/Coq.Lists.List.html *)",
      "",
      "Notation Int := Z .",
      ""
    )

  lazy val coq_recursive_function_prefixes : Seq [String] =
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

  lazy val coq_reserved_words =
    coq_1 .++ (coq_2 .++ (coq_3 .++ (coq_4) ) )

  lazy val coq_1 : Seq [String] =
    Seq (
      "as",
      "else",
      "end",
      "forall",
      "fun",
      "if",
      "in",
      "let",
      "match",
      "then",
      "with"
    )

  lazy val coq_2 : Seq [String] =
    Seq (
      "Admitted",
      "Arguments",
      "Check",
      "Constructors",
      "End",
      "Eval",
      "Export",
      "Hint",
      "Implicit",
      "Import",
      "Module",
      "Notation",
      "Print",
      "Proof",
      "Qed",
      "Require",
      "Resolve",
      "Section",
      "Set",
      "Type",
      "Unset"
    )

  lazy val coq_3 : Seq [String] =
    Seq (
      "admit",
      "apply",
      "assert",
      "auto",
      "case",
      "compute",
      "destruct",
      "discriminate",
      "elim",
      "exact",
      "induction",
      "intros",
      "pose",
      "refine",
      "rewrite",
      "simpl",
      "specialize",
      "unfold"
    )

  lazy val coq_4 : Seq [String] =
    Seq (
      "CoFixpoint",
      "CoInductive",
      "Definition",
      "Example",
      "Fixpoint",
      "Global",
      "Hypothesis",
      "Inductive",
      "Instance",
      "Lemma",
      "Ltac",
      "Parameter",
      "Theorem",
      "Variable"
    )

  lazy val type_symbols_translation : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (soda_constant .subtype_reserved_word , coq_subtype_symbol) ,
      Tuple2 (soda_constant .supertype_reserved_word , coq_supertype_symbol) ,
      Tuple2 (soda_constant .function_arrow_symbol , coq_function_arrow_symbol) ,
      Tuple2 (soda_constant .opening_bracket_symbol , coq_opening_parenthesis + coq_space) ,
      Tuple2 (soda_constant .closing_bracket_symbol , coq_space + coq_closing_parenthesis)
    )

  lazy val function_symbols_translation : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (soda_constant .function_definition_symbol , coq_function_definition_symbol) ,
      Tuple2 (soda_constant .lambda_reserved_word , coq_lambda_reserved_word) ,
      Tuple2 (soda_constant .any_reserved_word , coq_lambda_reserved_word) ,
      Tuple2 (soda_constant .lambda_arrow_symbol , coq_lambda_arrow_symbol) ,
      Tuple2 (soda_constant .case_arrow_symbol , coq_case_arrow_symbol) ,
      Tuple2 (soda_constant .not_reserved_word , coq_not_reserved_word) ,
      Tuple2 (soda_constant .and_reserved_word , coq_and_reserved_word) ,
      Tuple2 (soda_constant .or_reserved_word , coq_or_reserved_word) ,
      Tuple2 (soda_constant .equals_symbol , coq_equals_symbol) ,
      Tuple2 (soda_constant .less_than_symbol , coq_less_than_symbol) ,
      Tuple2 (soda_constant .less_than_or_equal_to_symbol , coq_less_than_or_equal_to_symbol) ,
      Tuple2 (soda_constant .tail_recursion_annotation , coq_empty_string) ,
      Tuple2 (soda_constant .override_annotation , coq_empty_string) ,
      Tuple2 (soda_constant .new_annotation , coq_empty_string)

    )

  lazy val type_translation : Seq [Tuple2 [String, String] ] =
    Seq (
        Tuple2 ("Boolean" , "bool"),
        Tuple2 ("Nat" , "nat"),
        Tuple2 ("Option" , "option"),
        Tuple2 ("List" , "list"),
        Tuple2 ("Nil" , "nil"),
        Tuple2 ("Seq" , "list"),
        Tuple2 ("String" , "string"),
        Tuple2 ("BigInt" , "Z"),
        Tuple2 ("Tuple2" , "prod")
    )

  lazy val prefix_coq_non_soda : String = "__soda__"

  lazy val coq_non_soda : Seq [Tuple2 [String, String] ] =
    coq_reserved_words
      .filter ( x => ! soda_constant .soda_reserved_words .contains (x))
      .map ( x => Tuple2 (x , prefix_coq_non_soda + x) )

  def is_coq_word (word : String) : Boolean =
    coq_reserved_words .contains (word)

}

case class TranslationConstantToCoq_ () extends TranslationConstantToCoq

object TranslationConstantToCoq {
  def mk : TranslationConstantToCoq =
    TranslationConstantToCoq_ ()
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
 * This translates Soda source code to Coq source code.
 */

trait TranslatorToCoq
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

  private lazy val _coq_extension : String = ".v"

  private lazy val _default_argument = "."

  private lazy val _translator =
    BlockProcessor .mk (
      DefaultBlockSequenceTranslator .mk (
        MicroTranslatorToCoq .mk
      )
    )

  private def _process_soda_file_with (pair : FileNamePair) : Boolean =
    _translate (pair .input_file_name) (pair .output_file_name)

  private def _get_input_output_file_names (input_name : String) : FileNamePair =
    if ( input_name .endsWith (_soda_extension)
    ) FileNamePair .mk (input_name) (
      input_name .substring (0, input_name .length - _soda_extension .length) + _coq_extension)
    else FileNamePair .mk (input_name + _soda_extension) (input_name + _coq_extension)

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

case class TranslatorToCoq_ () extends TranslatorToCoq

object TranslatorToCoq {
  def mk : TranslatorToCoq =
    TranslatorToCoq_ ()
}

