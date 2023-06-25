package soda.translator.extension.tocoq

/*
 * This package contains classes for the translation to Gallina,
 * the specification language used by Coq.
 */



trait Package

trait CoqClassConstructorBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToCoq_ ()

  private def _get_as_class_beginning_annotation (annotated_block : AnnotatedBlock)
      : Option [ClassBeginningAnnotation] =
    annotated_block match  {
      case ClassBeginningAnnotation_ (b) => Some (ClassBeginningAnnotation_ (b) )
      case otherwise => None
    }

  private def _get_class_beginning (referencess : Seq [AnnotatedBlock] )
      : Option [ClassBeginningAnnotation] =
    referencess
       .flatMap ( block => _get_as_class_beginning_annotation (block) )
       .headOption

  private def _remove_variable_with (line : String) (index : Int) : String =
    if ( index < 0
    ) line
    else line .substring (index + _sc .type_membership_symbol .length) .trim

  private def _remove_variable (line : String) : String =
    _remove_variable_with (line) (line .indexOf (_sc .type_membership_symbol) )

  private def _translate_type_symbols (line : String) : String =
    line
      .replaceAll (_sc .subtype_reserved_word , _tc .coq_subtype_symbol)
      .replaceAll (_sc .supertype_reserved_word , _tc .coq_supertype_symbol)
      .replaceAll (_sc .function_arrow_symbol , _tc .coq_function_arrow_symbol)

  private def _get_types_of_abstract_functions (block : AbstractDeclarationAnnotation) : Seq [String] =
    block .abstract_functions
      .map ( annotated_line => _translate_type_symbols (annotated_line .line) .trim )
      .map ( line => _remove_variable (line) )

  private def _get_first_line (block : AnnotatedBlock) : String =
    block .lines .headOption .getOrElse ("")

  private def _get_initial_spaces_with (line : String) : String =
    line .takeWhile ( ch => ch .isSpaceChar)

  private def _get_initial_spaces (block : AnnotatedBlock) : String =
    _get_initial_spaces_with (_get_first_line (block) )

  private def _get_constructor_declaration (beginning : ClassBeginningAnnotation)
      (functions : Seq [String] ) : String =
    _get_initial_spaces (beginning) +
    _tc .coq_inductive_reserved_word +
    _tc .coq_space +
    beginning .class_name +
    _tc .coq_space +
    _tc .coq_type_membership_symbol +
    _tc .coq_space +
    _tc .coq_type_reserved_word +
    _tc .coq_space +
    _tc .coq_function_definition_symbol +
    _tc .coq_new_line +
    _get_initial_spaces (beginning) +
    _tc .coq_space +
    _tc .coq_space +
    _tc .coq_vertical_bar_symbol +
    _tc .coq_space +
    beginning .class_name +
    _sc .constructor_suffix +
    _tc .coq_space +
    _tc .coq_opening_parenthesis +
    _tc .coq_some_variable_name +
    _tc .coq_space +
    _tc .coq_type_membership_symbol +
    _tc .coq_space +
    functions .mkString (_tc .coq_space + _tc .coq_product_type_symbol + _tc .coq_space) +
    _tc .coq_closing_parenthesis +
    _tc .coq_new_line +
    _get_initial_spaces (beginning) +
    _tc .coq_inductive_end_symbol

  private def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation)
      (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    AbstractDeclarationAnnotation_ (
      BlockBuilder_ () .build (
        Seq (_tc .coq_opening_comment) .++ (
          block .lines .++ (
            Seq [String] (
            _tc .coq_closing_comment,
            "",
            _get_constructor_declaration (beginning) (_get_types_of_abstract_functions (block) )
            )
          )
        )
      ),
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
        _translate_block (AbstractDeclarationAnnotation_ (block , references) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqClassConstructorBlockTranslator_ () extends CoqClassConstructorBlockTranslator


trait CoqClassDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.block.Translator
  import   soda.translator.blocktr.TableTranslator_
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.replacement.Replacement_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToCoq_ ()

  lazy val soda_space : String = _sc .space

  lazy val scala_space : String = " "

  def get_first_line (block : Block) : String =
    block .lines .headOption .getOrElse ("")

  def get_initial_spaces_for (line : String) : String =
    line .takeWhile ( ch => ch .isSpaceChar)

  def get_initial_spaces (block : Block) : String =
    get_initial_spaces_for (get_first_line (block) )

  private def _process_after_extends (block : Block) : Seq [String] =
    if ( (get_first_line (block) .trim .nonEmpty)
    ) block .lines .map ( line =>
       _tc .coq_import_reserved_word + _tc .coq_space + line .trim +
       _tc .coq_space + _tc .coq_end_symbol)
    else Seq [String] ()

  private def _remove_first_line (lines : Seq [String] ) : Seq [String] =
    if ( lines .isEmpty
    ) lines
    else lines .tail

  def remove_first_line (block : Block) : Block =
    BlockBuilder_ () .build (_remove_first_line (block .lines) )

  private def _process_if_extends (block : Block) : Seq [String] =
    if ( (get_first_line (block) .trim == _sc .extends_reserved_word)
    ) Seq [String] (
      get_initial_spaces (block) ) .++ (_process_after_extends (remove_first_line (block) ) )
    else block .lines

  private def _process_tail (block : Block) : Seq [String] =
    _process_if_extends (remove_first_line (block) )

  def get_table_translator (line : String) : Translator =
    TableTranslator_ (
      Seq (Tuple2 (_sc .class_reserved_word, _tc .coq_module_reserved_word ) )
    )

  private def _process_head_with (line : String) (block : Block) : Seq [String] =
    Seq [String] (
      Replacement_ (_sc .space + line)
        .replace_at_beginning (0) (get_table_translator (line) )
        .line .substring (_sc .space .length) + _tc .coq_space + _tc .coq_end_symbol
    )

  private def _process_head (block : Block) : Seq [String] =
    _process_head_with (get_first_line (block) ) (block)

  def contains_equals (line : String) : Boolean =
    line .trim .contains (_sc .function_definition_symbol)

  def has_condition_for_type_alias (line : String) : Boolean =
    contains_equals (line)

  private def _translate_block (block : AnnotatedBlock) : Block =
    if ( (has_condition_for_type_alias (get_first_line (block) ) )
    ) block
    else
      BlockBuilder_ () .build (
        _process_head (block) ++ _process_tail (block)
      )

  private def _translate_class_beginning_block (block : ClassBeginningAnnotation)
      : ClassBeginningAnnotation =
    ClassBeginningAnnotation_ (_translate_block (block) )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassBeginningAnnotation_ (block) =>
        _translate_class_beginning_block (ClassBeginningAnnotation_ (block) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqClassDeclarationBlockTranslator_ () extends CoqClassDeclarationBlockTranslator


trait CoqClassEndBlockTranslator
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

  private lazy val _tc = TranslationConstantToCoq_ ()

  private def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation)
      (block : ClassEndAnnotation) : ClassEndAnnotation =
    ClassEndAnnotation_ (
      BlockBuilder_ () .build (
        Seq [String] (
          _tc .coq_module_end_reserved_word + _tc .coq_space + beginning .class_name +
          _tc .coq_space + _tc .coq_end_symbol ,
          "",
          _tc .coq_import_reserved_word + _tc .coq_space + beginning .class_name +
          _tc .coq_space + _tc .coq_end_symbol
        )
      ),
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
      case otherwise => None
    }

  private def _translate_block (block : ClassEndAnnotation) : ClassEndAnnotation =
    _translate_block_with (_get_class_beginning (block.references) ) (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassEndAnnotation_ (block , references) =>
        _translate_block (ClassEndAnnotation_ (block , references) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqClassEndBlockTranslator_ () extends CoqClassEndBlockTranslator


trait CoqDefinitionBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_

  private lazy val _tc = TranslationConstantToCoq_ ()

  private def _append (suffix : String) (block : Block) : Block =
    BlockBuilder_ () .build (
      block .lines .:+ (suffix)
    )

  private def _prepend (prefix : String) (block : Block) : Block =
    BlockBuilder_ () .build (
      Seq[String] (prefix + block .lines .head) ++ block .lines .tail
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
    block .lines .headOption .getOrElse ("") .trim

  def is_a_recursive_definition (block : Block) : Boolean =
    _tc .coq_recursive_function_prefixes .exists (
       prefix => first_line (block) .startsWith (prefix)
    )

  private def _translate_block (block : FunctionDefinitionAnnotation) : Block =
    if ( is_a_recursive_definition (block)
    ) _append (_tc .coq_recursive_definition_end_symbol) (
      _prepend (_tc .coq_recursive_definition_reserved_word + _tc .coq_space) (block) )
    else _translate_non_recursive_definition (block)

  private def _translate_definition_block (block : FunctionDefinitionAnnotation)
      : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation_ (_translate_block (block) )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case FunctionDefinitionAnnotation_ (block) =>
        _translate_definition_block (FunctionDefinitionAnnotation_ (block) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqDefinitionBlockTranslator_ () extends CoqDefinitionBlockTranslator


trait CoqImportDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation_

  private lazy val _tc = TranslationConstantToCoq_ ()

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
    BlockBuilder_ () .build (
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
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqImportDeclarationBlockTranslator_ () extends CoqImportDeclarationBlockTranslator


trait CoqPackageDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.PackageDeclarationAnnotation
  import   soda.translator.parser.annotation.PackageDeclarationAnnotation_

  private lazy val _tc = TranslationConstantToCoq_ ()

  private def _comment_block (block : Block) : Block =
    BlockBuilder_ () .build (
      ( (Seq (_tc .coq_opening_comment) .++ (block .lines) ) .++ (
        Seq (_tc .coq_closing_comment) ) ) .++ (_tc .coq_prelude)
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
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqPackageDeclarationBlockTranslator_ () extends CoqPackageDeclarationBlockTranslator


trait CoqProofBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ProofBlockAnnotation
  import   soda.translator.parser.annotation.ProofBlockAnnotation_

  private lazy val _tc = TranslationConstantToCoq_ ()

  private def _append (suffix : String) (block : Block) : Block =
    BlockBuilder_ () .build (
      block .lines .:+ (suffix)
    )

  private def _get_tail_or_empty (sequence : Seq [String] ) : Seq [String] =
    if ( sequence .isEmpty
    ) sequence
    else sequence .tail

  private def _replace_first_line (first_line : String) (block : Block) : Block =
    BlockBuilder_ () .build (
      Seq (first_line)  .++ (_get_tail_or_empty (block .lines) )
    )

  private def _translate_block (block : ProofBlockAnnotation) : ProofBlockAnnotation =
    ProofBlockAnnotation_ (
      _append (
        _tc .coq_proof_end_reserved_word) (
          _replace_first_line (_tc .coq_proof_begin_reserved_word) (block)
      )
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ProofBlockAnnotation_ (block) => _translate_block (ProofBlockAnnotation_ (block) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqProofBlockTranslator_ () extends CoqProofBlockTranslator


trait CoqTheoremBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.TheoremBlockAnnotation
  import   soda.translator.parser.annotation.TheoremBlockAnnotation_

  private lazy val _tc = TranslationConstantToCoq_ ()

  private def _prepend (prefix : String) (block : Block) : Block =
    BlockBuilder_ () .build (
      Seq[String] (prefix + block .lines .head) .++ (block .lines .tail)
    )

  private def _append (suffix : String) (block : Block) : Block =
    BlockBuilder_ () .build (
      block .lines .:+ (suffix)
    )

  private def _remove_first_line (block : Block) : Block =
    if ( block .lines .isEmpty
    ) block
    else BlockBuilder_ () .build (block .lines .tail)

  private def _translate_block (block : TheoremBlockAnnotation) : TheoremBlockAnnotation =
    TheoremBlockAnnotation_ (
      _append (
        _tc .coq_theorem_end_symbol) (_prepend (
          _tc .coq_theorem_begin_reserved_word) (_remove_first_line (block)
        )
      )
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case TheoremBlockAnnotation_ (block) =>
        _translate_block (TheoremBlockAnnotation_ (block) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class CoqTheoremBlockTranslator_ () extends CoqTheoremBlockTranslator


/**
 * A line containing the definition sign will be classified as a definition.
 * The definitions need to be identified as 'val', 'def', or 'class'.
 *
 * 'class' is for class definition.
 * It is detected if the 'class' reserved word is also in the same line.
 *
 * 'val' is for value definition.
 * It is detected in three cases.
 * Case 1: The line does not have a opening parenthesis, e.g. `a = 1`
 * Case 2: The first opening parenthesis is after the definition sign, e.g. `x = f (y)`
 * Case 3: The first opening parenthesis is after a colon,
 *   e.g. `x : (A, B) -> C = (x, y) -> f (x, y)`
 * Case 4: The first non-blank character of a line is an opening parenthesis,
 *   e.g. `(x, y) = (0, 1)`
 *
 * 'def' is for function definition.
 * If it does not fit in any of the 'val' cases.
 *
 * Formerly there was another case for 'val'.
 * Deprecated Case:
 * This was implemented simply as:
 * `line.trim.startsWith (soda_opening_parenthesis)`
 * This is no longer supported.
 *
 */

trait DefinitionLineTranslator
  extends
    soda.translator.block.LineTranslator
{

  def   line : String

  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.replacement.Replacement
  import   soda.translator.replacement.Replacement_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToCoq_ ()

  private lazy val _trimmed_line : String = line .trim

  def get_index_from (line : String) (pattern : String) (start : Int) : OptionSD [Int] =
    SomeSD_ (line .indexOf (pattern, start) )
      .filter ( position => !  (position == -1) )

  def get_index (line : String) (pattern : String) : OptionSD [Int] =
    get_index_from (line) (pattern) (0)

  private lazy val _position_of_first_opening_parenthesis : OptionSD [Int] =
    get_index (line) (_sc .opening_parenthesis_symbol)

  private lazy val _is_val_definition_case_1 : Boolean =
    _position_of_first_opening_parenthesis .isEmpty

  private def _is_val_definition_case_2 (initial_position : Int) : Boolean =
    _position_of_first_opening_parenthesis match  {
      case SomeSD_ (position) => (position > initial_position)
      case otherwise => false
    }

  private lazy val _is_val_definition_case_3 : Boolean =
    (get_index (line) (_sc .type_membership_symbol) ) match  {
      case SomeSD_ (other_position) => _is_val_definition_case_2 (other_position)
      case otherwise => false
    }

  private lazy val _is_val_definition_case_4 : Boolean =
    _trimmed_line .startsWith (_sc .opening_parenthesis_symbol)

  private def _is_val_definition (initial_position : Int) : Boolean =
    _is_val_definition_case_1 ||
    _is_val_definition_case_2 (initial_position) ||
    _is_val_definition_case_3 ||
    _is_val_definition_case_4

  private lazy val _is_class_definition : Boolean =
    get_index (line) (_sc .space + _sc .class_reserved_word + _sc .space) .isDefined

  private lazy val _ends_with_equals = false

  private lazy val _ends_with_opening_brace = false

  private lazy val _contains_equals : Boolean =
    _trimmed_line .contains (_sc .function_definition_symbol)

  private lazy val _condition_for_type_alias : Boolean =
    _contains_equals && ! (_ends_with_equals || _ends_with_opening_brace)

  private lazy val _translation_of_class_definition : Replacement =
    if ( _condition_for_type_alias
    ) Replacement_ (line)
    else Replacement_ (line) .replace_all (_sc .space + _sc .function_definition_symbol) ("")

  private lazy val _translation_of_val_definition : Replacement =
    Replacement_ (line) .add_after_spaces_or_pattern (_tc .coq_space) (_tc .coq_space)

  private lazy val _translation_of_def_definition : Replacement =
    Replacement_ (line) .add_after_spaces_or_pattern (_tc .coq_space) (_tc .coq_space)

  private def _decide_val_or_def_translation (position : Int) : Replacement =
    if ( _is_val_definition (position)
    ) _translation_of_val_definition
    else _translation_of_def_definition

  private def _try_found_definition (position : Int) : Replacement =
    if ( _is_class_definition
    ) _translation_of_class_definition
    else _decide_val_or_def_translation (position)

  /**
   * A line is a definition when its main operator is "="  (the equals sign),
   * which in this context is also called the definition sign.
   * This function finds the first occurrence of the definition sign, if it is present.
   *
   * @param line line
   * @return maybe the position of the definition sign
   */

  def find_definition (line : String) : OptionSD [Int] =
    if ( line .endsWith (_sc .space + _sc .function_definition_symbol)
    ) SomeSD_ (line .length - _sc .function_definition_symbol .length)
    else get_index (line) (_sc .space + _sc .function_definition_symbol + _sc .space)

  lazy val translation : String =
    find_definition (line) match  {
      case SomeSD_ (position) => _try_found_definition (position) .line
      case otherwise => line
    }

}

case class DefinitionLineTranslator_ (line : String) extends DefinitionLineTranslator


trait MatchCaseBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_
  import   soda.translator.parser.annotation.TestDeclarationAnnotation
  import   soda.translator.parser.annotation.TestDeclarationAnnotation_
  import   soda.translator.replacement.ReplacementAux_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _soda_case_pattern = _sc .case_reserved_word + _sc .space

  private lazy val _tc = TranslationConstantToCoq_ ()

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
      ReplacementAux_ ()
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
    BlockBuilder_ () .build (
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
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class MatchCaseBlockTranslator_ () extends MatchCaseBlockTranslator


/**
 * This class translates Soda snippets into Coq snippets.
 */

trait MicroTranslatorToCoq
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.BlockTranslatorPipeline_
  import   soda.translator.block.ConditionalBlockTranslator_
  import   soda.translator.blocktr.TokenReplacement_
  import   soda.translator.blocktr.TokenizedBlockTranslator_
  import   soda.translator.replacement.Token

  private lazy val _tc = TranslationConstantToCoq_ ()

  private lazy val _function_definition = BlockAnnotationEnum_ () .function_definition

  private lazy val _test_declaration = BlockAnnotationEnum_ () .test_declaration

  lazy val functions_and_tests = Seq (_function_definition, _test_declaration)

  lazy val try_definition : Token => String =
     token =>
      DefinitionLineTranslator_ (token .text) .translation

  private lazy val _translation_pipeline =
    BlockTranslatorPipeline_ (
      Seq (
        MatchCaseBlockTranslator_ (),
        CoqDefinitionBlockTranslator_ (),
        CoqClassConstructorBlockTranslator_ (),
        CoqClassDeclarationBlockTranslator_ (),
        CoqPackageDeclarationBlockTranslator_ (),
        CoqClassEndBlockTranslator_ (),
        CoqImportDeclarationBlockTranslator_ (),
        CoqTheoremBlockTranslator_ (),
        CoqProofBlockTranslator_ (),
        ConditionalBlockTranslator_ (functions_and_tests,
          TokenizedBlockTranslator_ (try_definition) ),
        ConditionalBlockTranslator_ (functions_and_tests,
          TokenReplacement_ () .replace (_tc .function_symbols_translation) )
      )
    )

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _translation_pipeline .translate (block)

}

case class MicroTranslatorToCoq_ () extends MicroTranslatorToCoq


/**
 * This class contains constants that are specific for the Soda translator,
 * like reserved words for Soda and Coq.
 */

trait TranslationConstantToCoq
{

  import   soda.translator.parser.SodaConstant_

  lazy val soda_constant = SodaConstant_ ()

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

  lazy val coq_opening_comment = "(*"

  lazy val coq_closing_comment = "*)"

  lazy val coq_opening_documentation = "(**"

  lazy val coq_closing_documentation = "*)"

  lazy val coq_some_variable_name = "x"

  lazy val coq_opening_brace = "{"

  lazy val coq_closing_brace = "}"

  lazy val coq_product_type_symbol = "*"

  lazy val coq_lambda_reserved_word = "fun"

  lazy val coq_lambda_arrow_symbol = "=>"

  lazy val coq_case_arrow_symbol = "=>"

  lazy val coq_case_translation = coq_vertical_bar_symbol + coq_space

  lazy val coq_not_reserved_word = "notb"

  lazy val coq_and_reserved_word = "andb"

  lazy val coq_or_reserved_word = "orb"

  lazy val coq_end_symbol = "."

  lazy val coq_definition_reserved_word : String = "Definition"

  lazy val coq_inductive_reserved_word : String = "Inductive"

  lazy val coq_set_reserved_word : String = "Set"

  lazy val coq_type_reserved_word : String = "Type"

  lazy val coq_module_reserved_word : String = "Module"

  lazy val coq_module_end_reserved_word : String = "End"

  lazy val coq_import_reserved_word : String = "Import"

  lazy val coq_recursive_definition_reserved_word : String = "Fixpoint"

  lazy val coq_inductive_end_symbol : String = coq_end_symbol

  lazy val coq_definition_end_symbol : String = coq_end_symbol

  lazy val coq_recursive_definition_end_symbol : String = coq_end_symbol

  lazy val coq_with_reserved_word : String = "with"

  lazy val coq_theorem_begin_reserved_word : String = "Theorem"

  lazy val coq_theorem_end_symbol : String = coq_end_symbol

  lazy val coq_proof_begin_reserved_word : String = "Proof."

  lazy val coq_proof_end_reserved_word : String = "Qed."

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
    coq_1 ++ coq_2 ++ coq_3 ++ coq_4

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
      Tuple2 (soda_constant.subtype_reserved_word, coq_subtype_symbol),
      Tuple2 (soda_constant.supertype_reserved_word, coq_supertype_symbol),
      Tuple2 (soda_constant.function_arrow_symbol, coq_function_arrow_symbol)
    )

  lazy val function_symbols_translation : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (soda_constant.function_definition_symbol , coq_function_definition_symbol),
      Tuple2 (soda_constant.lambda_reserved_word , coq_lambda_reserved_word),
      Tuple2 (soda_constant.any_reserved_word , coq_lambda_reserved_word),
      Tuple2 (soda_constant.lambda_arrow_symbol , coq_lambda_arrow_symbol),
      Tuple2 (soda_constant.case_arrow_symbol , coq_case_arrow_symbol),
      Tuple2 (soda_constant.not_reserved_word , coq_not_reserved_word),
      Tuple2 (soda_constant.and_reserved_word , coq_and_reserved_word),
      Tuple2 (soda_constant.or_reserved_word , coq_or_reserved_word)
    )

  lazy val type_translation : Seq [ Tuple2 [String, String]  ] =
    Seq (
        Tuple2 ("Boolean" , "bool"),
        Tuple2 ("Nat" , "nat"),
        Tuple2 ("Option" , "option"),
        Tuple2 ("List" , "list"),
        Tuple2 ("String" , "string"),
        Tuple2 ("BigInt" , "Z")
    )

  lazy val prefix_coq_non_soda : String = "__soda__"

  lazy val coq_non_soda : Seq [Tuple2 [String, String] ] =
    coq_reserved_words
      .filter ( x => ! soda_constant.soda_reserved_words .contains (x))
      .map ( x => Tuple2 (x , prefix_coq_non_soda + x) )

  def is_coq_word (word : String) : Boolean =
    coq_reserved_words .contains (word)

  def is_soda_word (word : String) : Boolean =
    soda_constant .soda_reserved_words.contains (word)

}

case class TranslationConstantToCoq_ () extends TranslationConstantToCoq


trait FileNamePair
{

  def   input_file_name : String
  def   output_file_name : String

}

case class FileNamePair_ (input_file_name : String, output_file_name : String) extends FileNamePair

/**
 * This translates Soda source code to Coq source code.
 */

trait TranslatorToCoq
  extends
    soda.translator.extension.common.Extension
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.io.DirectoryProcessor_
  import   soda.translator.io.SimpleFileReader_
  import   soda.translator.io.SimpleFileWriter_
  import   soda.translator.parser.BlockProcessor_
  import   java.io.File

  private lazy val _soda_extension : String = ".soda"

  private lazy val _coq_extension : String = ".v"

  private lazy val _default_argument = "."

  private lazy val _translator =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToCoq_ ()
      )
    )

  private def _process_soda_file_with (pair : FileNamePair) : Boolean =
    _translate (pair .input_file_name) (pair .output_file_name)

  private def _get_input_output_file_names (input_name : String) : FileNamePair =
    if ( input_name .endsWith (_soda_extension)
    ) FileNamePair_ (input_name,
      input_name .substring (0, input_name .length - _soda_extension .length) + _coq_extension)
    else FileNamePair_ (input_name + _soda_extension, input_name + _coq_extension)

  private def _process_soda_file (file : File) : Boolean =
    _process_soda_file_with (_get_input_output_file_names (file .getAbsolutePath) )

  private def _process_directory (start : String) : Boolean =
    DirectoryProcessor_ (start , _process_soda_file) .process ()

  private def _translate_with_input (input : String) (output_file_name : String) : Boolean =
    SimpleFileWriter_ () .write_file (
      output_file_name) (
      content = _translator .translate (input)
    )

  private def _translate (input_file_name : String) (output_file_name : String) : Boolean =
    _translate_with_input (
      SimpleFileReader_ () .read_file (input_file_name) ) (
      output_file_name
    )

  def execute_for (arguments : Seq [String] ) : Boolean =
    arguments.length match  {
      case 0 => _process_directory (_default_argument)
      case 1 => _process_directory (arguments (0) )
      case 2 => _translate (arguments (0) ) (arguments (1) )
      case otherwise => false
    }

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      execute_for (arguments)

}

case class TranslatorToCoq_ () extends TranslatorToCoq

