package soda.translator.extension.tolean

/*
 * This package contains classes for the translation to Lean.
 */



trait Package

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

  private lazy val _tc = TranslationConstantToLean_ ()

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
    Replacement_ (line) .add_after_spaces_or_pattern (_tc .lean_space) (_tc .lean_space)

  private lazy val _translation_of_def_definition : Replacement =
    Replacement_ (line) .add_after_spaces_or_pattern (_tc .lean_space) (_tc .lean_space)

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


trait LeanClassConstructorBlockTranslator
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

  private lazy val _tc = TranslationConstantToLean_ ()

  private def _get_as_class_beginning_annotation (annotated_block : AnnotatedBlock)
      : Option [ClassBeginningAnnotation] =
    annotated_block match  {
      case ClassBeginningAnnotation_ (b) => Some (ClassBeginningAnnotation_ (b) )
      case otherwise => None
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

  private def _get_default_constructor_name (beginning : ClassBeginningAnnotation) : String =
    beginning .class_name + _sc .constructor_suffix

  private lazy val _two_spaces : String = _tc .lean_space + _tc .lean_space

  private lazy val _four_spaces : String = _two_spaces + _two_spaces

  private def _get_constructor_declaration (beginning : ClassBeginningAnnotation)
     (functions : Seq [String] ) : String =
    _get_initial_spaces (beginning) +
    _tc .lean_class_reserved_word +
    _tc .lean_space +
    beginning .class_name +
    _tc .lean_space +
    _tc .lean_where_reserved_word +
    _tc .lean_new_line + _get_initial_spaces (beginning) + _two_spaces +
    _get_default_constructor_name (beginning) +
    _tc .lean_space +
    _tc .lean_constructor_symbol +
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
    AbstractDeclarationAnnotation_ (
      BlockBuilder_ () .build (
        Seq [String] (
          _get_constructor_declaration (beginning) (_get_types_of_abstract_functions (block) ) )
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

case class LeanClassConstructorBlockTranslator_ () extends LeanClassConstructorBlockTranslator


trait LeanClassDeclarationBlockTranslator
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

  private lazy val _tc = TranslationConstantToLean_ ()

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
    ) block .lines .map ( line => _tc .lean_import_reserved_word + _tc .lean_space +
      line .trim + _tc .lean_space + _tc .lean_end_symbol)
    else Seq [String] ()

  private def _remove_first_line (lines : Seq [String] ) : Seq [String] =
    if ( lines .isEmpty
    ) lines
    else lines .tail

  def remove_first_line (block : Block) : Block =
    BlockBuilder_ () .build (_remove_first_line (block .lines) )

  private def _process_if_extends (block : Block) : Seq [String] =
    if ( (get_first_line (block) .trim == _sc .extends_reserved_word)
    ) Seq [String] (get_initial_spaces (block) ) .++ (
      _process_after_extends (remove_first_line (block) ) )
    else block .lines

  private def _process_tail (block : Block) : Seq [String] =
    _process_if_extends (remove_first_line (block) )

  def get_table_translator (line : String) : Translator =
    TableTranslator_ (
      Seq (Tuple2 (_sc .class_reserved_word, _tc .lean_namespace_reserved_word ) )
    )

  private def _process_head_with (line : String) (block : Block) : Seq [String] =
    Seq [String] (
      Replacement_ (_sc .space + line)
        .replace_at_beginning (0) (get_table_translator (line) )
        .line .substring (_sc .space .length)
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

case class LeanClassDeclarationBlockTranslator_ () extends LeanClassDeclarationBlockTranslator


trait LeanClassEndBlockTranslator
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

  private lazy val _tc = TranslationConstantToLean_ ()

  private def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation)
      (block : ClassEndAnnotation) : ClassEndAnnotation =
    ClassEndAnnotation_ (
      BlockBuilder_ () .build (
        Seq [String] (
          _tc .lean_namespace_end_reserved_word + _tc .lean_space + beginning .class_name ,
          "",
          _tc .lean_open_reserved_word + _tc .lean_space + beginning .class_name
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

case class LeanClassEndBlockTranslator_ () extends LeanClassEndBlockTranslator


trait LeanDefinitionBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_

  private lazy val _tc = TranslationConstantToLean_ ()

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
    ! _tc .non_definition_block_prefixes .exists ( prefix =>
      block .contents .trim .startsWith (prefix) )

  private def _translate_non_recursive_definition (block : FunctionDefinitionAnnotation) : Block =
    if ( is_a_definition (block)
    ) _append (_tc .lean_definition_end_symbol) (_prepend (
      _tc .lean_def_reserved_word + _tc .lean_space) (block) )
    else block

  def first_line (block : Block) : String =
    block .lines .headOption .getOrElse ("") .trim

  def is_a_recursive_definition (block : Block) : Boolean =
    _tc .lean_recursive_function_prefixes .exists ( prefix =>
      first_line (block) .startsWith (prefix) )

  private def _translate_block (block : FunctionDefinitionAnnotation) : Block =
    if ( is_a_recursive_definition (block)
    ) _append (_tc .lean_recursive_definition_end_symbol) (_prepend (
      _tc .lean_recursive_definition_reserved_word + _tc .lean_space) (block) )
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

case class LeanDefinitionBlockTranslator_ () extends LeanDefinitionBlockTranslator


trait LeanImportDeclarationBlockTranslator
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

  private lazy val _tc = TranslationConstantToLean_ ()

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
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class LeanImportDeclarationBlockTranslator_ () extends LeanImportDeclarationBlockTranslator


trait LeanPackageDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.PackageDeclarationAnnotation
  import   soda.translator.parser.annotation.PackageDeclarationAnnotation_

  private lazy val _tc = TranslationConstantToLean_ ()

  private def _comment_block (block : Block) : Block =
    BlockBuilder_ () .build (
      ( (Seq (_tc .lean_opening_comment) .++ (block .lines) ) .++ (
        Seq (_tc .lean_closing_comment) ) ) .++ (_tc .lean_prelude)
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

case class LeanPackageDeclarationBlockTranslator_ () extends LeanPackageDeclarationBlockTranslator


trait LeanProofBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ProofBlockAnnotation
  import   soda.translator.parser.annotation.ProofBlockAnnotation_

  private lazy val _tc = TranslationConstantToLean_ ()

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
        _tc .lean_proof_end_reserved_word) (
          _replace_first_line (_tc .lean_proof_begin_reserved_word) (block)
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

case class LeanProofBlockTranslator_ () extends LeanProofBlockTranslator


trait LeanTheoremBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.TheoremBlockAnnotation
  import   soda.translator.parser.annotation.TheoremBlockAnnotation_

  private lazy val _tc = TranslationConstantToLean_ ()

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
        _tc .lean_theorem_end_symbol) (_prepend (
          _tc .lean_theorem_reserved_word) (_remove_first_line (block)
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

case class LeanTheoremBlockTranslator_ () extends LeanTheoremBlockTranslator


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

  private lazy val _tc = TranslationConstantToLean_ ()

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
      ReplacementAux_ ()
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
    BlockBuilder_ () .build (
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
 * This class translates Soda snippets into Lean snippets.
 */

trait MicroTranslatorToLean
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

  private lazy val _tc = TranslationConstantToLean_ ()

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
        LeanDefinitionBlockTranslator_ (),
        LeanClassConstructorBlockTranslator_ (),
        LeanClassDeclarationBlockTranslator_ (),
        LeanPackageDeclarationBlockTranslator_ (),
        LeanClassEndBlockTranslator_ (),
        LeanImportDeclarationBlockTranslator_ (),
        LeanTheoremBlockTranslator_ (),
        LeanProofBlockTranslator_ (),
        ConditionalBlockTranslator_ (functions_and_tests,
          TokenizedBlockTranslator_ (try_definition) ),
        ConditionalBlockTranslator_ (functions_and_tests,
          TokenReplacement_ () .replace (_tc .function_symbols_translation)
        )
      )
    )

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _translation_pipeline .translate (block)

}

case class MicroTranslatorToLean_ () extends MicroTranslatorToLean


/**
 * This class contains constants that are specific for the Soda translator, like reserved words for Soda and Lean.
 */

trait TranslationConstantToLean
{

  import   soda.translator.parser.SodaConstant_

  lazy val soda_constant = SodaConstant_ ()

  lazy val lean_space = " "

  lazy val lean_new_line = "\n"

  lazy val lean_function_definition_symbol = ":="

  lazy val lean_type_membership_symbol = ":"

  lazy val lean_subtype_symbol = "<:"

  lazy val lean_supertype_symbol = ">:"

  lazy val lean_function_arrow_symbol = "->"

  lazy val lean_empty_string = ""

  lazy val lean_vertical_bar_symbol = "|"

  lazy val lean_match_end_translation = ""

  lazy val lean_opening_parenthesis = "("

  lazy val lean_closing_parenthesis = ")"

  lazy val lean_opening_comment = "/-"

  lazy val lean_closing_comment = "-/"

  lazy val lean_opening_documentation = "/--"

  lazy val lean_closing_documentation = "-/"

  lazy val lean_some_variable_name = "x"

  lazy val lean_opening_brace = "{"

  lazy val lean_closing_brace = "}"

  lazy val lean_product_type_symbol = "*"

  lazy val lean_constructor_symbol = "::"

  lazy val lean_lambda_arrow_symbol = "=>"

  lazy val lean_case_arrow_symbol = "=>"

  lazy val lean_case_translation = lean_vertical_bar_symbol + lean_space

  lazy val lean_not_reserved_word = "notb"

  lazy val lean_and_symbol = "&&"

  lazy val lean_or_symbol = "||"

  lazy val lean_end_symbol = ""

  lazy val lean_set_reserved_word : String = "Set"

  lazy val lean_type_reserved_word : String = "Type"

  lazy val lean_decidable_eq_type_name : String = "DecidableEq"

  lazy val lean_inductive_end_symbol : String = lean_end_symbol

  lazy val lean_definition_end_symbol : String = lean_end_symbol

  lazy val lean_recursive_definition_end_symbol : String = lean_end_symbol

  lazy val lean_theorem_end_symbol : String = lean_end_symbol

  lazy val lean_by_reserved_word : String = "by"

  lazy val lean_calc_reserved_word : String = "calc"

  lazy val lean_class_reserved_word : String = "class"

  lazy val lean_def_reserved_word : String = "def"

  lazy val lean_deriving_reserved_word : String = "deriving"

  lazy val lean_do_reserved_word : String = "do"

  lazy val lean_else_reserved_word : String = "else"

  lazy val lean_end_reserved_word : String = "end"

  lazy val lean_example_reserved_word : String = "example"

  lazy val lean_fun_reserved_word = "fun"

  lazy val lean_if_reserved_word : String = "if"

  lazy val lean_import_reserved_word : String = "import"

  lazy val lean_in_reserved_word : String = "in"

  lazy val lean_inductive_reserved_word : String = "inductive"

  lazy val lean_infix_reserved_word : String = "infix"

  lazy val lean_instance_reserved_word : String = "instance"

  lazy val lean_let_reserved_word : String = "let"

  lazy val lean_match_reserved_word : String = "match"

  lazy val lean_namespace_reserved_word : String = "namespace"

  lazy val lean_notation_reserved_word : String = "notation"

  lazy val lean_open_reserved_word : String = "open"

  lazy val lean_set_option_reserved_word : String = "set_option"

  lazy val lean_structure_reserved_word : String = "structure"

  lazy val lean_then_reserved_word : String = "then"

  lazy val lean_theorem_reserved_word : String = "theorem"

  lazy val lean_where_reserved_word : String = "where"

  lazy val lean_with_reserved_word : String = "with"

  lazy val lean_hash_check_reserved_word : String = "#check"

  lazy val lean_hash_eval_reserved_word : String = "#eval"

  lazy val lean_hash_print_reserved_word : String = "#print"

  lazy val lean_hash_reduce_reserved_word : String = "#reduce"

  lazy val lean_recursive_definition_reserved_word : String = lean_def_reserved_word

  lazy val lean_namespace_end_reserved_word : String = lean_end_reserved_word

  lazy val lean_proof_begin_reserved_word : String = ""

  lazy val lean_proof_end_reserved_word : String = lean_end_reserved_word

  lazy val lean_reserved_words : Seq [String] =
    Seq (
      lean_by_reserved_word ,
      lean_calc_reserved_word ,
      lean_class_reserved_word ,
      lean_def_reserved_word ,
      lean_deriving_reserved_word ,
      lean_do_reserved_word,
      lean_else_reserved_word ,
      lean_end_reserved_word,
      lean_example_reserved_word ,
      lean_fun_reserved_word ,
      lean_if_reserved_word ,
      lean_import_reserved_word ,
      lean_in_reserved_word ,
      lean_inductive_reserved_word ,
      lean_infix_reserved_word ,
      lean_instance_reserved_word ,
      lean_let_reserved_word ,
      lean_match_reserved_word  ,
      lean_namespace_reserved_word ,
      lean_notation_reserved_word ,
      lean_open_reserved_word ,
      lean_set_option_reserved_word ,
      lean_structure_reserved_word ,
      lean_then_reserved_word ,
      lean_theorem_reserved_word ,
      lean_where_reserved_word ,
      lean_with_reserved_word ,
      lean_hash_check_reserved_word ,
      lean_hash_eval_reserved_word ,
      lean_hash_print_reserved_word ,
      lean_hash_reduce_reserved_word
    )

  lazy val lean_prelude : Seq [String] =
    Seq (
      ""
    )

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
      Tuple2 (soda_constant .subtype_reserved_word, lean_subtype_symbol),
      Tuple2 (soda_constant .supertype_reserved_word, lean_supertype_symbol),
      Tuple2 (soda_constant .function_arrow_symbol, lean_function_arrow_symbol)
    )

  lazy val function_symbols_translation : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (soda_constant .function_definition_symbol , lean_function_definition_symbol),
      Tuple2 (soda_constant .lambda_reserved_word , lean_fun_reserved_word),
      Tuple2 (soda_constant .any_reserved_word , lean_fun_reserved_word),
      Tuple2 (soda_constant .lambda_arrow_symbol , lean_lambda_arrow_symbol),
      Tuple2 (soda_constant .case_arrow_symbol , lean_case_arrow_symbol),
      Tuple2 (soda_constant .not_reserved_word , lean_not_reserved_word),
      Tuple2 (soda_constant .and_reserved_word , lean_and_symbol),
      Tuple2 (soda_constant .or_reserved_word , lean_or_symbol)
    )

  lazy val type_translation : Seq [ Tuple2 [String, String]  ] =
    Seq (
        Tuple2 ("Boolean" , "Bool"),
        Tuple2 ("Nat" , "Nat"),
        Tuple2 ("Option" , "Option"),
        Tuple2 ("List" , "List"),
        Tuple2 ("String" , "String"),
        Tuple2 ("Tuple2" , "Prod")
    )

  lazy val prefix_lean_non_soda : String = "__soda__"

  lazy val lean_non_soda : Seq [Tuple2 [String, String] ] =
    lean_reserved_words
      .filter ( x => ! soda_constant .soda_reserved_words  .contains (x))
      .map ( x => Tuple2 (x , prefix_lean_non_soda + x) )

  def is_lean_word (word : String) : Boolean =
    lean_reserved_words  .contains (word)

  def is_soda_word (word : String) : Boolean =
    soda_constant .soda_reserved_words .contains (word)

}

case class TranslationConstantToLean_ () extends TranslationConstantToLean


trait FileNamePair
{

  def   input_file_name : String
  def   output_file_name : String

}

case class FileNamePair_ (input_file_name : String, output_file_name : String) extends FileNamePair

/**
 * This translates Soda source code to Lean source code.
 */

trait TranslatorToLean
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

  private lazy val _lean_extension : String = ".lean"

  private lazy val _default_argument = "."

  private lazy val _translator =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToLean_ ()
      )
    )

  private def _process_soda_file_with (pair : FileNamePair) : Boolean =
    _translate (pair .input_file_name) (pair .output_file_name)

  private def _get_input_output_file_names (input_name : String) : FileNamePair =
    if ( input_name .endsWith (_soda_extension)
    ) FileNamePair_ (input_name,
      input_name .substring (0, input_name .length - _soda_extension .length) + _lean_extension)
    else FileNamePair_ (input_name + _soda_extension, input_name + _lean_extension)

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

case class TranslatorToLean_ () extends TranslatorToLean

