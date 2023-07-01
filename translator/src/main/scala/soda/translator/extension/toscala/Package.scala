package soda.translator.extension.toscala

/*
 * This package contains classes for the translation to Scala.
 */



trait Package

trait AbstractDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.Block
  import   soda.translator.block.Block_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  lazy val scala_abstract_function_declaration_pattern =
    _tc .scala_abstract_function_declaration + _tc .scala_space

  private def _translate_type_parameters_in_line (line : String) : String =
    line
      .replaceAll (_sc .type_parameter_separation_regex,
         _tc .scala_type_parameter_separator_symbol + _tc .scala_space)

  private def _translate_type_parameters (abstract_functions_with_comments : Seq [AnnotatedLine] )
      : Seq [AnnotatedLine] =
    abstract_functions_with_comments
      .map ( annotated_line =>
        if ( annotated_line .is_comment
        ) annotated_line
        else AnnotatedLine_ (_translate_type_parameters_in_line (annotated_line .line) ,
          annotated_line .is_comment)
      )

  def get_number_of_spaces_at_beginning (line : String) : Int =
    line
      .takeWhile (ch => ch .isSpaceChar)
      .length

  def get_first_line (block : AnnotatedBlock) : String =
    block .lines .headOption .getOrElse ("")

  def prepend_aligned_non_comment (index : Int) (prefix : String) (annotated_line : AnnotatedLine)
      : AnnotatedLine =
    if ( annotated_line .is_comment
    ) annotated_line
    else AnnotatedLine_ (annotated_line .line .substring (0, index) + prefix +
      annotated_line .line .substring (index) , annotated_line .is_comment)

  def prepend_to_lines_aligned_at (number_of_spaces : Int) (prefix : String)
      (annotated_lines : Seq [AnnotatedLine] ) : Block =
    Block_ (
      annotated_lines .map ( annotated_line =>
        prepend_aligned_non_comment (number_of_spaces) (prefix) (annotated_line) )
    )

  private def _translate_block (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    AbstractDeclarationAnnotation_ (
      prepend_to_lines_aligned_at (
        get_number_of_spaces_at_beginning (get_first_line (block) ) ) (
        scala_abstract_function_declaration_pattern) (
        _translate_type_parameters (block .abstract_functions_with_comments)
      ),
      block .references
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case AbstractDeclarationAnnotation_ (block, references) =>
        _translate_block (AbstractDeclarationAnnotation_ (block, references) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class AbstractDeclarationBlockTranslator_ () extends AbstractDeclarationBlockTranslator


trait ClassConstructorBlockTranslator
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
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  private def _get_initial_spaces_with (line : String) : String =
    line .takeWhile ( ch => ch .isSpaceChar)

  private def _get_first_line (block : AnnotatedBlock) : String =
    block .lines .headOption .getOrElse ("")

  private def _get_initial_spaces (block : AnnotatedBlock) : String =
    _get_initial_spaces_with (_get_first_line (block) )

  private def _get_as_parameter_list (parameters : Seq [String] ) : String =
    if ( parameters .isEmpty
    ) ""
    else _tc .scala_space + _tc .scala_opening_bracket +
      parameters .mkString (_tc .scala_type_parameter_separator_symbol + _tc .scala_space) +
       _tc .scala_closing_bracket

  private def _get_constructor_declaration (beginning : ClassBeginningAnnotation)
      (functions : Seq [String] ) : String =
    _get_initial_spaces (beginning) +
    _tc .class_declaration_translation_at_beginning_with_paren +
    _tc .scala_space +
    beginning .class_name +
    _sc .constructor_suffix +
    _translate_type_symbols (_get_as_parameter_list (beginning .type_parameters_and_bounds) ) +
    _tc .scala_space +
    _tc .scala_opening_parenthesis +
    functions .mkString (_tc .scala_type_parameter_separator_symbol + _tc .scala_space) +
    _tc .scala_closing_parenthesis +
    _tc .scala_space +
    _tc .scala_extends_translation +
    _tc .scala_space +
    beginning .class_name +
    _get_as_parameter_list (beginning .type_parameters)

  private def _translate_type_symbols (line : String) : String =
    line
      .replaceAll (_sc .type_parameter_separation_regex ,
        _tc .scala_type_parameter_separator_symbol + _tc .scala_space)
      .replaceAll (_sc .subtype_reserved_word , _tc .scala_subtype_symbol)
      .replaceAll (_sc .supertype_reserved_word , _tc .scala_supertype_symbol)
      .replaceAll (_sc .function_arrow_symbol , _tc .scala_function_arrow_symbol)

  private def _get_as_abstract_declaration_annotation (block : AnnotatedBlock)
      : Option [AbstractDeclarationAnnotation] =
    block match  {
      case AbstractDeclarationAnnotation_ (b , references) =>
        Some (AbstractDeclarationAnnotation_ (b , references) )
      case otherwise => None
    }

  private def _get_abstract_functions (references : Seq [AnnotatedBlock] ) : Seq [String] =
    references
      .flatMap ( block => _get_as_abstract_declaration_annotation (block) )
      .flatMap ( block => block .abstract_functions)
      .map ( annotated_line => _translate_type_symbols (annotated_line .line) .trim )

  private def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation)
      (block : ClassEndAnnotation) : ClassEndAnnotation =
    ClassEndAnnotation_ (
      BlockBuilder_ () .build (
        block .lines .++ (
          Seq [String] (
            "",
            _get_constructor_declaration (beginning) (
              _get_abstract_functions (block .references) )
          )
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

  private def _translate_block (block : ClassEndAnnotation) : ClassEndAnnotation =
    _translate_block_with (_get_class_beginning (block .references) ) (block)

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

case class ClassConstructorBlockTranslator_ () extends ClassConstructorBlockTranslator


trait State
{

  def   index : Int
  def   last_index : Int
  def   bracket_level : Int
  def   par_level : Int
  def   line : String
  def   accum : String
  def   expecting : Boolean

  import   soda.translator.parser.SodaConstant_

  private def _mk_State (index : Int) (last_index : Int) (bracket_level : Int) (par_level : Int)
      (line : String) (accum : String) (expecting : Boolean) : State =
    State_ (index, last_index, bracket_level, par_level, line, accum, expecting)

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  private lazy val _opening_parenthesis_symbol_char = _sc .opening_parenthesis_symbol .head

  private lazy val _closing_parenthesis_symbol_char = _sc .closing_parenthesis_symbol .head

  private lazy val _opening_bracket_symbol_char = _sc .opening_bracket_symbol .head

  private lazy val _closing_bracket_symbol_char = _sc .closing_bracket_symbol .head

  private lazy val _update_opening_par : State =
    if ( (par_level == 0) && (expecting)
    ) _mk_State (index + 1) (index + 1) (bracket_level) (par_level + 1) (line) (
      accum + _tc.scala_class_parameter_separator_symbol + _tc .scala_space) (false)
    else _mk_State (index + 1) (last_index) (bracket_level) (par_level + 1) (line) (accum) (
      expecting)

  private lazy val _update_closing_par : State =
    if ( (par_level == 1)
    ) _mk_State (index + 1) (index) (bracket_level) (par_level - 1) (line) (
      accum + line .substring (last_index , index)) (true)
    else _mk_State (index + 1) (last_index) (bracket_level) (par_level - 1) (line) (accum) (
      expecting)

  private lazy val _update_opening_bracket : State =
    _mk_State (index + 1) (last_index) (bracket_level + 1) (par_level) (line) (accum) (
      expecting)

  private lazy val _update_closing_bracket : State =
    _mk_State (index + 1) (last_index) (bracket_level - 1) (par_level) (line) (accum) (
      expecting)

  private lazy val _update_next_space : State =
    _mk_State (line .length) (line .length) (bracket_level) (par_level) (line) (
      accum + line .substring (last_index) ) (expecting)

  private lazy val _update_default_step : State =
    _mk_State (index + 1) (last_index) (bracket_level) (par_level) (line) (accum) (expecting)

  lazy val compute_next : State =
    if ( (index >= line .length)
    ) this
    else if ( (line .charAt (index) == _opening_parenthesis_symbol_char)
    ) _update_opening_par
    else if ( (line .charAt (index) == _closing_parenthesis_symbol_char)
    ) _update_closing_par
    else if ( (line .charAt (index) == _opening_bracket_symbol_char)
    ) _update_opening_bracket
    else if ( (line .charAt (index) == _closing_bracket_symbol_char)
    ) _update_closing_bracket
    else if ( (bracket_level == 0) && (par_level == 0) && (
      ! (line .charAt (index) == _sc .space .head) )
    ) _update_next_space
    else _update_default_step

}

case class State_ (index : Int, last_index : Int, bracket_level : Int, par_level : Int, line : String, accum : String, expecting : Boolean) extends State

trait ClassConstructorParameterBlockTranslator
  extends
    soda.translator.blocktr.TokenizedBlockTranslator
{

  import   soda.translator.parser.SodaConstant_
  import   soda.translator.replacement.Token

  private def _mk_State (index : Int) (last_index : Int) (bracket_level : Int) (par_level : Int)
      (line : String) (accum : String) (expecting : Boolean) : State =
    State_ (index, last_index, bracket_level, par_level, line, accum, expecting)

  lazy val range = soda.lib.Range_ ()

  lazy val fold = soda.lib.Fold_ ()

  lazy val fold_while = soda.lib.FoldWhile_ ()

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  private def _translate_line_initial (line : String) (index : Int) : State =
    _mk_State (index = index) (last_index = index) (bracket_level = 0) (par_level = 0) (
      line = line) (accum = line .substring (0 , index) ) (expecting = false)

  private def _translate_line_next (a : State) (ch : Char) : State =
    a .compute_next

  private def _translate_line_with_parentheses_with_tuple (a : State) : String =
    a .accum + a .line .substring (a .last_index)

  private def _translate_line (line : String) (index : Int) : String =
    _translate_line_with_parentheses_with_tuple (
      fold (line) (_translate_line_initial (line) (index) ) (_translate_line_next) )

  private def _translate_line_with_parentheses_after_constr (line : String) (from_index : Int) : String =
    _translate_line (line) (line .indexOf (_sc .constructor_suffix + _sc .space , from_index) +
       _sc .constructor_suffix .length)

  private def _translate_line_with_parentheses_after_multiple_constructors (line :  String)
      (occurrences : Seq [Int] ) : String =
    fold (occurrences) (line) (
       accum =>
         from_index =>
          _translate_line_with_parentheses_after_constr (accum) (from_index)
    )

  private def _replace_parentheses_by_comma_with (line : String) (occurrences : Seq [Int] ) : String =
    if ( (occurrences .length >= 0)
    ) _translate_line_with_parentheses_after_multiple_constructors (line) (occurrences)
    else line

  private def _get_next_position (line : String) (indices : Seq [Int] ) : Int =
    if ( indices .isEmpty
    ) line .indexOf (_sc .constructor_suffix + _sc .space)
    else line .indexOf (_sc .constructor_suffix + _sc .space , indices .head +
      _sc .constructor_suffix .length)

  private def _find_occurrences_in_reverse_order (line : String) : Seq [Int] =
    fold_while (range (line .length) ) (Seq [Int] () ) (
       accum =>
         index =>
          accum .+: (_get_next_position (line) (accum) )
    ) (
       accum =>
         index =>
          _get_next_position (line) (accum) >= 0
    )

  private def _replace_parentheses_by_comma (line : String) : String =
    _replace_parentheses_by_comma_with (line) (_find_occurrences_in_reverse_order (line) )

  lazy val replace_token : Token => String =
     token =>
      _replace_parentheses_by_comma (token .text)

}

case class ClassConstructorParameterBlockTranslator_ () extends ClassConstructorParameterBlockTranslator


trait ClassDeclarationBlockTranslator
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
  import   soda.translator.parser.annotation.ClassAliasAnnotation
  import   soda.translator.parser.annotation.ClassAliasAnnotation_
  import   soda.translator.replacement.Replacement_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  lazy val soda_space : String = _sc .space

  def ends_with_equals (line : String) : Boolean = false

  def ends_with_opening_brace (line : String) : Boolean = false

  def contains_equals (line : String) : Boolean =
    line .trim .contains (_sc .function_definition_symbol)

  def has_condition_for_type_alias (line : String) : Boolean =
    contains_equals (line)

  def get_class_declaration_translation (line : String) : String =
    if ( line .contains (_sc .opening_parenthesis_symbol)
    ) _tc .class_declaration_translation_at_beginning_with_paren
    else
      if ( has_condition_for_type_alias (line)
      ) _tc .class_declaration_translation_at_beginning_without_paren_for_type_alias
      else _tc .class_declaration_translation_at_beginning_without_paren

  def get_table_translator (line : String) : Translator =
    TableTranslator_ (
      Seq (Tuple2 (_sc .class_reserved_word, get_class_declaration_translation (line) ) )
    )

  def get_first_line (lines : Seq [String] ) : String =
    lines .headOption .getOrElse ("")

  def get_initial_spaces_for (line : String) : String =
    line .takeWhile ( ch => ch .isSpaceChar)

  def get_initial_spaces (lines : Seq [String] ) : String =
    get_initial_spaces_for (get_first_line (lines) )

  def remove_first_line (lines : Seq [String] ) : Seq [String] =
    if ( lines .isEmpty
    ) lines
    else lines .tail

  private def _process_after_extends (lines : Seq [String] ) : Seq [String] =
    if ( (get_first_line (lines) .trim .nonEmpty)
    ) Seq [String] (get_first_line (lines) ) ++ remove_first_line (lines)
      .map ( line => get_initial_spaces_for (line) + _tc .scala_with_translation +
        _tc .scala_space + line .trim)
    else Seq [String] ()

  private def _process_head_with (line : String)  : Seq [String] =
    Seq [String] (
      Replacement_ (_sc .space + line)
       .replace_at_beginning (0) (get_table_translator (line) )
       .line .substring (_sc .space .length)
    )

  private def _process_head (lines : Seq [String] ) : Seq [String] =
    _process_head_with (get_first_line (lines) )

  private def _process_if_extends (lines : Seq [String] ) : Seq [String] =
    if ( (get_first_line (lines) .trim == _sc .extends_reserved_word)
    ) Seq [String] (get_initial_spaces (lines) + _tc .scala_extends_translation) .++ (
      _process_after_extends (remove_first_line (lines) ) )
    else lines

  private def _process_tail (lines : Seq [String] ) : Seq [String] =
    _process_if_extends (remove_first_line (lines) )

  private def _translate_block_with (lines : Seq [String] ) : Seq [String] =
    if ( (has_condition_for_type_alias (get_first_line (lines) ) )
    ) _process_head (lines) .++ (_process_tail (lines) )
    else _process_head (lines) .++ (_process_tail (lines) .++ (Seq [String] (
      get_initial_spaces (lines) + _tc .scala_class_begin_symbol) ) )

  private def _remove_type_annotation_in_line (lines : Seq [String] ) : Seq [String] =
    Seq [String] (
      get_first_line (lines)
        .replaceAll (_sc .main_type_membership_regex , "")
    )

  def remove_type_annotation (lines : Seq [String] ) : Seq [String] =
    _remove_type_annotation_in_line (lines) .++ (remove_first_line (lines) )

  private def _translate_block (block : AnnotatedBlock) : Block =
    BlockBuilder_ () .build (
      remove_type_annotation (
        _translate_block_with (
          block .lines
        )
      )
    )

  private def _translate_class_beginning_block (block : ClassBeginningAnnotation)
      : ClassBeginningAnnotation =
    ClassBeginningAnnotation_ (_translate_block (block) )

  private def _translate_class_alias_block (block : ClassAliasAnnotation) : ClassAliasAnnotation =
    ClassAliasAnnotation_ (_translate_block (block) )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassBeginningAnnotation_ (block) =>
        _translate_class_beginning_block (ClassBeginningAnnotation_ (block) )
      case ClassAliasAnnotation_ (block) =>
        _translate_class_alias_block (ClassAliasAnnotation_ (block) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class ClassDeclarationBlockTranslator_ () extends ClassDeclarationBlockTranslator


trait ClassEndBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  private def _mk_ClassEndAnnotation (block : Block) (references : Seq [AnnotatedBlock] )
      : ClassEndAnnotation =
    ClassEndAnnotation_ (block, references)

  private lazy val _tc = TranslationConstantToScala_ ()

  private def _get_first_line (block : AnnotatedBlock) : String =
    block .lines .headOption .getOrElse ("")

  private def _get_initial_spaces_with (line : String) : String =
    line .takeWhile ( ch => ch .isSpaceChar)

  private def _get_initial_spaces (block : ClassEndAnnotation) : String =
    _get_initial_spaces_with (_get_first_line (block) )

  private def _get_translation (block : ClassEndAnnotation) : String =
    _get_initial_spaces (block) + _tc .scala_class_end_symbol

  private def _translate_block (block : ClassEndAnnotation) : ClassEndAnnotation =
    _mk_ClassEndAnnotation (
      BlockBuilder_ () .build (
        Seq [String] (
          _get_translation (block)
        )
      ) ) (
      block .references
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassEndAnnotation_ (block, references) =>
        _translate_block (_mk_ClassEndAnnotation (block) (references) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class ClassEndBlockTranslator_ () extends ClassEndBlockTranslator


trait FunctionDefinitionBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.Block
  import   soda.translator.block.Block_
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_
  import   soda.translator.replacement.Replacement_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  private def _prepend_line (line : String) (block : Block) : Block =
    BlockBuilder_ () .build (
      Seq [String] (line) .++ (block .lines)
    )

  private def _is_annotation (line : String) : Boolean =
    (line .trim == _sc .tail_recursion_annotation) || (line .trim == _sc .override_annotation)

  private def _private_prefix_if_necessary (line : String) : String =
    if ( line .trim .startsWith (_sc .private_function_prefix)
    ) _tc .scala_private_reserved_word + _tc .scala_space
    else ""

  private def _translate_val_definition (line : String) : String =
    Replacement_ (line)
      .add_after_spaces_or_pattern (_tc .scala_space) (_private_prefix_if_necessary (line) +
        _tc .scala_value + _tc .scala_space)
      .line

  private def _translate_def_definition (line : String) : String =
    Replacement_ (line)
      .add_after_spaces_or_pattern (_tc .scala_space) (_private_prefix_if_necessary (line) +
        _tc .scala_definition + _tc .scala_space)
      .line

  private def _replace_first_line (lines : Seq [AnnotatedLine] ) (new_first_line : String)
      : Seq [AnnotatedLine] =
    if ( lines .isEmpty
    ) Seq [AnnotatedLine] () .+: (AnnotatedLine_ (new_first_line , false) )
    else lines .tail .+: (AnnotatedLine_ (new_first_line , false) )

  private def _replace_on_val_block (initial_comments : Seq [AnnotatedLine] )
      (main_block : Seq [AnnotatedLine] ) : Block =
    Block_ (
      initial_comments .++ (_replace_first_line (main_block) (
        _translate_val_definition (main_block .head .line) ) )
    )

  private def _replace_on_def_block (initial_comments : Seq [AnnotatedLine] )
      (main_block : Seq [AnnotatedLine] ) : Block =
    Block_ (
      initial_comments .++ (_replace_first_line (main_block) (
        _translate_def_definition (main_block .head .line) ) )
    )

  private def _get_initial_comment (lines : Seq [AnnotatedLine] ) : Seq [AnnotatedLine] =
    lines .takeWhile ( annotated_line => annotated_line .is_comment)

  private def _get_part_without_initial_comment (lines : Seq [AnnotatedLine] ) : Seq [AnnotatedLine] =
    lines .dropWhile ( annotated_line => annotated_line .is_comment)

  def get_first_line (lines : Seq [String] ) : String =
    lines .headOption .getOrElse ("")

  def remove_first_line (lines : Seq [String] ) : Seq [String] =
    if ( lines .isEmpty
    ) lines
    else lines .tail

  private def _remove_type_annotation_in_line (lines : Seq [String] ) : Seq [String] =
    Seq [String] (
      get_first_line (lines)
        .replaceAll (_sc .main_type_membership_regex , "")
    )

  def remove_type_annotation (lines : Seq [String] ) : Seq [String] =
    _remove_type_annotation_in_line (lines) ++ remove_first_line (lines)

  private def _translate_main_block_with (block : Block) (detector : FunctionDefinitionLineDetector)
      : Block =
    detector .detect match  {
      case detector .val_detected =>
        _replace_on_val_block (_get_initial_comment (block .annotated_lines) ) (
          _get_part_without_initial_comment (block .annotated_lines) )
      case detector .def_detected =>
        _replace_on_def_block (_get_initial_comment (block .annotated_lines) ) (
          _get_part_without_initial_comment (block .annotated_lines) )
      case detector .def_reserved_word_detected => block
      case otherwise => block
    }

  private def _flatten_block (block : Block) : String =
    block .lines .mkString (_sc .space)

  private def _translate_main_block (block : Block) : Block =
    BlockBuilder_ () .build (
      remove_type_annotation (
        _translate_main_block_with (block) (
            FunctionDefinitionLineDetector_ (_flatten_block (block) ) )
          .lines
      )
    )

  private def _remove_first_line_if_possible (block : Block) : Block =
    if ( block .lines .isEmpty
    ) block
    else BlockBuilder_ () .build (block .lines .tail)

  private def _translate_block_with (first_line : AnnotatedLine) (block : Block) : Block =
    if ( _is_annotation (first_line .line)
    ) _prepend_line (first_line .line) (_translate_main_block (
      _remove_first_line_if_possible (block) ) )
    else _translate_main_block (block)

  private def _translate_block (block : Block) : Block =
    if ( block .readable_lines .isEmpty
    ) block
    else _translate_block_with (block .readable_lines .head) (block)

  private def _translate_function_definition_block (block : Block) : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation_ (_translate_block (block) )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case FunctionDefinitionAnnotation_ (block) =>
        _translate_function_definition_block (block)
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class FunctionDefinitionBlockTranslator_ () extends FunctionDefinitionBlockTranslator


/**
 * A line containing the definition sign will be classified as a definition.
 * The definitions need to be identified as 'val' case, 'def' case, or 'def' reserved word.
 *
 * When the 'def' reserved word is not detected at the beginning of the line, the following cases need to be determined.
 *
 * 'val' is for value definition.
 * It is detected in three cases.
 * Case 1: The line does not have a opening parenthesis, e.g. `a = 1`
 * Case 2: The first opening parenthesis is after the definition sign, e.g. `x = f (y)`
 * Case 3: The first opening parenthesis or bracket is after a colon,
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

trait FunctionDefinitionLineDetector
{

  def   line : String

  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_
  import   soda.translator.parser.SodaConstant_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _trimmed_line : String = line.trim

  lazy val undetected = 0

  lazy val val_detected = 1

  lazy val def_detected = 2

  lazy val def_reserved_word_detected = 3

  private def _get_index_from (line : String) (pattern : String) (start : Int) : OptionSD [Int] =
    SomeSD_ (line .indexOf (pattern, start) )
       .filter ( position => ! (position == -1) )

  private def _get_index (line : String) (pattern : String) : OptionSD [Int] =
    _get_index_from (line) (pattern) (0)

  private lazy val _position_of_first_opening_parenthesis : OptionSD [Int] =
    _get_index (line) (_sc .opening_parenthesis_symbol)

  private lazy val _position_of_first_opening_bracket : OptionSD [Int] =
    _get_index (line) (_sc .opening_bracket_symbol)

  private def _min (a : Int) (b : Int) : Int =
    if ( a < b
    ) a
    else b

  private def _position_of_first_opening_parenthesis_or_bracket_with (index1 : Int) : Int =
    _position_of_first_opening_bracket match  {
      case SomeSD_ (index2) => _min (index1) (index2)
      case otherwise => index1
    }

  private lazy val _position_of_first_opening_parenthesis_or_bracket : OptionSD [Int] =
    _position_of_first_opening_parenthesis match  {
      case SomeSD_ (index1) =>
        SomeSD_ (_position_of_first_opening_parenthesis_or_bracket_with (index1) )
      case otherwise => _position_of_first_opening_bracket
    }

  private lazy val _is_val_definition_case_1 : Boolean =
    _position_of_first_opening_parenthesis .isEmpty

  private def _is_val_definition_case_2 (initial_position : Int) : Boolean =
    _position_of_first_opening_parenthesis match  {
      case SomeSD_ (position) => (position > initial_position)
      case otherwise => false
    }

  private def _is_val_definition_case_2_b (initial_position : Int) : Boolean =
    _position_of_first_opening_parenthesis_or_bracket match  {
      case SomeSD_ (position) => (position > initial_position)
      case otherwise => false
    }

  private lazy val _is_val_definition_case_3 : Boolean =
    (_get_index (line) (_sc .type_membership_symbol) ) match  {
      case SomeSD_ (other_position) => _is_val_definition_case_2_b (other_position)
      case otherwise => false
    }

  private lazy val _is_val_definition_case_4 : Boolean =
    _trimmed_line .startsWith (_sc .opening_parenthesis_symbol)

  private def _is_val_definition (initial_position : Int) : Boolean =
    _is_val_definition_case_1 ||
    _is_val_definition_case_2 (initial_position) ||
    _is_val_definition_case_3 ||
    _is_val_definition_case_4

  private def _try_found_definition_without_def_reserved_word (position : Int) : Int =
    if ( _is_val_definition (position)
    ) val_detected
    else def_detected

  private def _starts_with_def_reserved_word (position : Int) : Boolean =
    line .trim .startsWith (_sc .def_reserved_word + _sc .space)

  private def _try_found_definition (position : Int) : Int =
    if ( _starts_with_def_reserved_word (position)
    ) def_reserved_word_detected
    else _try_found_definition_without_def_reserved_word (position)

  /**
   * A line is a definition when its main operator is "="  (the equals sign),
   * which in this context is also called the definition sign .
   * This function finds the first occurrence of the definition sign, if it is present.
   *
   * @param line line
   * @return maybe the position of the definition sign
   */

  private def _find_definition (line : String) : OptionSD [Int] =
    if ( line .endsWith (_sc .space + _sc .function_definition_symbol)
    ) SomeSD_ (line .length - _sc .function_definition_symbol .length)
    else _get_index (line) (_sc .space + _sc .function_definition_symbol + _sc .space)

  lazy val detect : Int =
    _find_definition (line) match  {
      case SomeSD_ (position) => _try_found_definition (position)
      case otherwise => undetected
    }

}

case class FunctionDefinitionLineDetector_ (line : String) extends FunctionDefinitionLineDetector


trait ImportDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation
  import   soda.translator.parser.annotation.ImportDeclarationAnnotation_

  private lazy val _tc = TranslationConstantToScala_ ()

  lazy val scala_import_declaration_pattern =
    _tc.scala_import_declaration + _tc.scala_space

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
        scala_import_declaration_pattern) (
        block .imported_items
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

case class ImportDeclarationBlockTranslator_ () extends ImportDeclarationBlockTranslator


trait MainClassBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  private def _mk_ClassEndAnnotation (block : Block) (references : Seq [AnnotatedBlock] )
      : ClassEndAnnotation =
    ClassEndAnnotation_ (block, references)

  private lazy val _tc = TranslationConstantToScala_ ()

  private def _get_as_class_beginning_annotation (block : AnnotatedBlock) : Option [ClassBeginningAnnotation] =
    block match  {
      case ClassBeginningAnnotation_ (b) => Some (ClassBeginningAnnotation_ (b) )
      case otherwise => None
    }

  private def _get_class_beginning (references : Seq [AnnotatedBlock] ) : Option [ClassBeginningAnnotation] =
    references
      .flatMap ( block => _get_as_class_beginning_annotation (block) )
      .headOption

  private def _get_class_name (references : Seq [AnnotatedBlock] ) : String =
    _get_class_beginning (references)
      .map ( x => x .class_name)
      .getOrElse ("")

  private def _translate_block (block : ClassEndAnnotation) : ClassEndAnnotation =
    if ( _get_class_name (block .references) == _tc .soda_main_class_name
    )
      _mk_ClassEndAnnotation (
        BlockBuilder_ () .build (
          Seq [String] (
            _tc .scala_class_end_symbol,
            "",
            _tc .scala_entry_point
          )
        ) ) (
        block .references
      )
    else block

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassEndAnnotation_ (block, references) =>
        _translate_block (_mk_ClassEndAnnotation (block) (references) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class MainClassBlockTranslator_ () extends MainClassBlockTranslator


trait MatchCaseBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.ClassAliasAnnotation
  import   soda.translator.parser.annotation.ClassAliasAnnotation_
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_
  import   soda.translator.parser.annotation.TestDeclarationAnnotation
  import   soda.translator.parser.annotation.TestDeclarationAnnotation_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  private lazy val _soda_match_pattern = _sc .match_reserved_word + " "

  private def _left_part (index : Int) (line : String) : String =
    line .substring (0 , index)

  private def _right_part (index : Int) (line : String) : String =
    line .substring (index + _soda_match_pattern .length , line .length)

  private def _assemble_parts (index : Int) (line : String) : String =
    (_left_part (index) (line) ) + (_right_part (index) (line) ) +
      _tc .scala_match_translation + _tc .scala_space + _tc .scala_opening_brace

  private def _is_a_match_line (line : String) : Boolean =
    line .trim .startsWith (_soda_match_pattern)

  private def _is_a_match_case_structure (block : AnnotatedBlock) : Boolean =
    block .lines .exists ( line => _is_a_match_line (line) )

  private def _get_tabulation_of_match (block : AnnotatedBlock) : String =
    block .lines
      .find ( line => _is_a_match_line (line) )
      .map ( line => _left_part (line .indexOf (_soda_match_pattern) ) (line) )
      .getOrElse (_tc .scala_space)

  private def _insert_match_before_brace_if_found (line : String) : String =
    if ( _is_a_match_line (line)
    ) _assemble_parts (index = line .indexOf (_soda_match_pattern) ) (line)
    else line

  private def _translate_match_case_structure (block: AnnotatedBlock) (tabulation : String) : Block =
    BlockBuilder_ () .build (
      block .lines
        .map ( line => _insert_match_before_brace_if_found (line) )
        .++ (Seq [String] () .+: (tabulation + _tc .scala_match_end_translation) )
    )

  private def _translate_block (block : AnnotatedBlock) : Block =
    if ( _is_a_match_case_structure (block)
    ) _translate_match_case_structure (block) (_get_tabulation_of_match (block) )
    else block

  private def _translate_function_block (block : AnnotatedBlock) : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation_ (_translate_block (block) )

  private def _translate_class_alias_block (block : AnnotatedBlock) : ClassAliasAnnotation =
    ClassAliasAnnotation_ (_translate_block (block) )

  private def _translate_test_block (block : AnnotatedBlock) : TestDeclarationAnnotation =
    TestDeclarationAnnotation_ (_translate_block (block) )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case FunctionDefinitionAnnotation_ (block) => _translate_function_block (annotated_block)
      case ClassAliasAnnotation_ (block) => _translate_class_alias_block (annotated_block)
      case TestDeclarationAnnotation_ (block) => _translate_test_block (annotated_block)
      case otherwise => annotated_block
    }

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class MatchCaseBlockTranslator_ () extends MatchCaseBlockTranslator


/**
 * This class translates Soda source code into Scala source code.
 */

trait MicroTranslatorToScala
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockTranslatorPipeline_
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.ConditionalBlockTranslator_
  import   soda.translator.blocktr.TokenReplacement_

  private lazy val _tc = TranslationConstantToScala_ ()

  private lazy val _ba = BlockAnnotationEnum_ ()

  private lazy val _functions_and_tests =
    Seq (_ba .function_definition , _ba .class_alias , _ba .test_declaration)

  private lazy val _class_declarations =
    Seq (_ba .class_alias , _ba .class_beginning , _ba .abstract_declaration)

  private lazy val _definitions_and_declarations =
    _functions_and_tests .++ (_class_declarations)

  private lazy val _translation_pipeline =
    BlockTranslatorPipeline_ (
      Seq (
        TypeParameterBlockTranslator_ () ,
        ClassConstructorParameterBlockTranslator_ () ,
        MatchCaseBlockTranslator_ () ,
        ConditionalBlockTranslator_ (_definitions_and_declarations ,
          TokenReplacement_ () .replace (_tc .scala_non_soda) ) ,
        ConditionalBlockTranslator_ (_functions_and_tests ,
          FunctionDefinitionBlockTranslator_ () ) ,
        ConditionalBlockTranslator_ (_class_declarations ,
          TokenReplacement_ () .replace (_tc .type_symbol_translation) ) ,
        ConditionalBlockTranslator_ (_functions_and_tests ,
          TokenReplacement_ () .replace (_tc .all_translations) ) ,
        ClassDeclarationBlockTranslator_ () ,
        ImportDeclarationBlockTranslator_ () ,
        AbstractDeclarationBlockTranslator_ () ,
        TheoremBlockTranslator_ () ,
        ScalaDirectiveBlockTranslator_ () ,
        ClassEndBlockTranslator_ () ,
        MainClassBlockTranslator_ () ,
        ClassConstructorBlockTranslator_ ()
      )
    )

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      _translation_pipeline .translate (block)

}

case class MicroTranslatorToScala_ () extends MicroTranslatorToScala


trait ScalaDirectiveBlockTranslator
  extends
    soda.translator.blocktr.DirectiveBlockTranslator
{

  private lazy val _tc = TranslationConstantToScala_ ()

  lazy val identifier : String = _tc .scala_directive_identifier

  lazy val opening_comment : String = _tc .scala_comment_opening_symbol

  lazy val closing_comment : String = _tc .scala_comment_closing_symbol

}

case class ScalaDirectiveBlockTranslator_ () extends ScalaDirectiveBlockTranslator


trait TheoremBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.DirectiveBlockAnnotation
  import   soda.translator.parser.annotation.DirectiveBlockAnnotation_
  import   soda.translator.parser.annotation.TheoremBlockAnnotation
  import   soda.translator.parser.annotation.TheoremBlockAnnotation_

  private lazy val _tc = TranslationConstantToScala_ ()

  private def _prepend (prefix : String) (block : Block) : Block =
    BlockBuilder_ () .build (
      Seq [String] (prefix + block .lines .head) .++ (block .lines .tail)
    )

  private def _append (suffix : String) (block : Block) : Block =
    BlockBuilder_ () .build (
      block .lines .:+ (suffix)
    )

  private def _translate_block (block : AnnotatedBlock) : Block =
    _append (_tc .scala_comment_closing_symbol) (_prepend (
      _tc .scala_comment_opening_symbol) (block) )

  private def _translate_theorem_block (block : AnnotatedBlock) : TheoremBlockAnnotation =
    TheoremBlockAnnotation_ (_translate_block (block) )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case TheoremBlockAnnotation_ (block) =>
        _translate_theorem_block (TheoremBlockAnnotation_ (block) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class TheoremBlockTranslator_ () extends TheoremBlockTranslator


/**
 * This class contains constants that are specific for the Soda translator,
 * like reserved words for Soda and Scala.
 */

trait TranslationConstantToScala
{

  import   soda.translator.parser.SodaConstant_

  lazy val soda_constant = SodaConstant_ ()

  lazy val scala_3_class_definition = ":"

  lazy val scala_match_translation = " match "

  lazy val scala_space = " "

  lazy val scala_empty_string = ""

  lazy val scala_comma = ","

  lazy val scala_type_parameter_separator_symbol = ","

  lazy val scala_class_parameter_separator_symbol = ","

  lazy val scala_lambda_arrow_symbol = "=>"

  lazy val scala_case_arrow_symbol = "=>"

  lazy val scala_parameter_definition_symbol = "="

  lazy val scala_match_end_translation = "}"

  lazy val scala_opening_parenthesis = "("

  lazy val scala_closing_parenthesis = ")"

  lazy val scala_opening_bracket = "["

  lazy val scala_closing_bracket = "]"

  lazy val scala_opening_brace = "{"

  lazy val scala_closing_brace = "}"

  lazy val scala_class_begin_symbol = "{"

  lazy val scala_class_end_symbol = "}"

  lazy val scala_comment_opening_symbol = "/*"

  lazy val scala_comment_closing_symbol = "*/"

  lazy val scala_if_translation = "if ("

  lazy val scala_then_translation = ")"

  lazy val scala_abstract_function_declaration = "def"

  lazy val scala_definition = "def"

  lazy val scala_value = "lazy val"

  lazy val scala_import_declaration = "import"

  lazy val scala_with_translation = "with"

  lazy val scala_extends_translation = "extends"

  lazy val scala_function_arrow_symbol = "=>"

  lazy val scala_subtype_symbol = "<:"

  lazy val scala_supertype_symbol = ">:"

  lazy val scala_not_symbol = "!"

  lazy val scala_and_symbol = "&&"

  lazy val scala_or_symbol = "||"

  lazy val scala_tail_recursion_annotation_translation =
    "import scala.annotation.tailrec\n        @tailrec  final"

  lazy val scala_override_annotation_translation = "override"

  lazy val scala_new_annotation_translation = "new"

  lazy val soda_main_class_name = "Main"

  lazy val scala_entry_point =
    "object EntryPoint {\n  def main (args: Array [String]): Unit = Main_ ().main (args)\n}\n"

  lazy val scala_private_reserved_word = "private"

  lazy val scala_directive_identifier = "scala"

  /**
   * Scala 3 keywords:
   *   https://dotty.epfl.ch/docs/internals/syntax.html
   * Scala 2 keywords:
   *   https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html
   */
  lazy val scala_reserved_words =
    scala_3_regular_keywords ++ scala_3_soft_keywords ++ scala_2_extra_keywords

  lazy val scala_3_regular_keywords =
    Seq (
      "abstract",
      "case",
      "catch",
      "class",
      "def",
      "do",
      "else",
      "enum",
      "export",
      "extends",
      "false",
      "final",
      "finally",
      "for",
      "given",
      "if",
      "implicit",
      "import",
      "lazy",
      "match",
      "new",
      "null",
      "object",
      "override",
      "package",
      "private",
      "protected",
      "return",
      "sealed",
      "super",
      "then",
      "throw",
      "trait",
      "true",
      "try",
      "type",
      "val",
      "var",
      "while",
      "with",
      "yield",
      ":",
      "=",
      "<-",
      "=>",
      "<:",
      ">:",
      "#",
      "@",
      "=>>",
      "?=>"
    )

  lazy val scala_3_soft_keywords =
    Seq (
      "as",
      "derives",
      "end",
      "extension",
      "infix",
      "inline",
      "opaque",
      "open",
      "transparent",
      "using",
      "|",
      "*",
      "+",
      "-"
    )

  lazy val scala_2_extra_keywords =
    Seq (
      "forSome",
      "macro",
      "this",
      "_",
      "<%",
      "\u21D2",
      "\u2190"
    )

  lazy val type_symbol_translation : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (soda_constant .subtype_reserved_word , scala_subtype_symbol) ,
      Tuple2 (soda_constant .supertype_reserved_word , scala_supertype_symbol) ,
      Tuple2 (soda_constant .function_arrow_symbol , scala_function_arrow_symbol)
    )

  lazy val function_symbol_translation : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (soda_constant .lambda_reserved_word , scala_empty_string) ,
      Tuple2 (soda_constant .any_reserved_word , scala_empty_string) ,
      Tuple2 (soda_constant .lambda_arrow_symbol , scala_lambda_arrow_symbol) ,
      Tuple2 (soda_constant .case_arrow_symbol , scala_case_arrow_symbol) ,
      Tuple2 (soda_constant .not_reserved_word , scala_not_symbol) ,
      Tuple2 (soda_constant .and_reserved_word , scala_and_symbol) ,
      Tuple2 (soda_constant .or_reserved_word , scala_or_symbol) ,
      Tuple2 (soda_constant .if_reserved_word , scala_if_translation) ,
      Tuple2 (soda_constant .then_reserved_word , scala_then_translation) ,
      Tuple2 (soda_constant .parameter_definition_symbol , scala_parameter_definition_symbol) ,
      Tuple2 (soda_constant .tail_recursion_annotation ,
        scala_tail_recursion_annotation_translation) ,
      Tuple2 (soda_constant .override_annotation , scala_override_annotation_translation) ,
      Tuple2 (soda_constant .new_annotation , scala_new_annotation_translation)
    )

  lazy val all_translations : Seq [Tuple2 [String, String] ] =
    type_symbol_translation .++ (function_symbol_translation)

  lazy val class_declaration_translation_at_beginning_with_paren = "case class"

  lazy val class_declaration_translation_at_beginning_without_paren_for_type_alias = "type"

  lazy val class_declaration_translation_at_beginning_without_paren = "trait"

  lazy val prefix_scala_non_soda = "__soda__"

  lazy val scala_non_soda : Seq [Tuple2 [String, String] ] =
      scala_reserved_words
        .filter ( x => ! soda_constant .soda_reserved_words .contains (x))
        .map ( x => Tuple2 (x , prefix_scala_non_soda + x) )

  def is_scala_word (word : String) : Boolean =
    scala_reserved_words .contains (word)

  def is_soda_word (word : String) : Boolean =
    soda_constant .soda_reserved_words .contains (word)

}

case class TranslationConstantToScala_ () extends TranslationConstantToScala


trait TranslatorToScalaConstant
{

  import   soda.translator.parser.BlockProcessor_
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   java.io.File

  lazy val soda_extension : String = ".soda"

  lazy val scala_extension : String = ".scala"

  lazy val default_argument = "."

  lazy val package_file_prefix : String = "Package"

  lazy val package_file_name : String = package_file_prefix + soda_extension

  lazy val package_scala_file_name : String = package_file_prefix + scala_extension

  lazy val file_separator : String = File .separator

  lazy val default_prelude : String = ""

  lazy val new_line : String = "\n"

  lazy val append_separation : String = new_line + new_line

  lazy val prelude_file_body : String = new_line + "trait Package" + append_separation

  lazy val single_files_option_1 = "-s"

  lazy val single_files_option_2 = "--single"

  lazy val translator =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToScala_ ()
      )
    )

}

case class TranslatorToScalaConstant_ () extends TranslatorToScalaConstant

trait FileNamePair
{

  def   input_file_name : String
  def   output_file_name : String

}

case class FileNamePair_ (input_file_name : String, output_file_name : String) extends FileNamePair

trait IndividualProcessor
{

  import   soda.translator.io.SimpleFileReader_
  import   soda.translator.io.SimpleFileWriter_
  import   java.io.File

  lazy val tc = TranslatorToScalaConstant_ ()

  lazy val reader = SimpleFileReader_ ()

  lazy val writer = SimpleFileWriter_ ()

  private def _mk_FileNamePair (input_file_name : String) (output_file_name : String) : FileNamePair =
    FileNamePair_ (input_file_name, output_file_name)

  private def _is_a_prelude_file (input_file_name : String) : Boolean =
    tc .package_file_name == ( ( new File (input_file_name) )  .getName)

  private def _get_prelude_with (prelude_file : File) : String =
    if ( prelude_file .exists
    ) (reader .read_file (prelude_file .getAbsolutePath) ) + tc .append_separation
    else tc .default_prelude

  private def _get_prelude_file (input_file_name : String) : File =
    new File ( new File (input_file_name)  .getParentFile , tc .package_file_name )

  private def _get_prelude (input_file_name : String) : String =
    _get_prelude_with (_get_prelude_file (input_file_name) )

  private def _read_input_with_prelude (input_file_name : String) : String =
    if ( _is_a_prelude_file (input_file_name)
    ) reader .read_file (input_file_name) + tc .prelude_file_body
    else _get_prelude (input_file_name) + reader .read_file (input_file_name)

  def get_input_output_file_names (input_name : String) : FileNamePair =
    if ( input_name .endsWith (tc .soda_extension)
    ) _mk_FileNamePair (input_name) (input_name .substring (0 ,
      input_name .length - tc .soda_extension .length) + tc .scala_extension)
    else _mk_FileNamePair (input_name + tc .soda_extension) (input_name + tc .scala_extension)

  private def _translate_with_input (input : String) (output_file_name : String) : Boolean =
    writer .write_file (output_file_name) (content = tc .translator .translate (input) )

  def translate (input_file_name : String) (output_file_name : String) : Boolean =
    _translate_with_input (_read_input_with_prelude (input_file_name) ) (output_file_name)

  def process_soda_file_with (pair : FileNamePair) : Boolean =
    translate (pair .input_file_name) (pair .output_file_name)

  def process_soda_file (file : File) : Boolean =
    process_soda_file_with (get_input_output_file_names (file .getAbsolutePath) )

}

case class IndividualProcessor_ () extends IndividualProcessor

trait PackageProcessor
{

  import   soda.translator.io.SimpleFileReader_
  import   soda.translator.io.SimpleFileWriter_
  import   java.io.File

  private def _mk_FileNamePair (input_file_name : String) (output_file_name : String) : FileNamePair =
    FileNamePair_ (input_file_name, output_file_name)

  lazy val tc = TranslatorToScalaConstant_ ()

  lazy val reader = SimpleFileReader_ ()

  lazy val writer = SimpleFileWriter_ ()

  def get_input_output_file_names (input_name : String) (parent_name : String) : FileNamePair =
    if ( input_name .endsWith (tc .soda_extension)
    ) _mk_FileNamePair (input_name) (parent_name + tc .file_separator +
      tc .package_scala_file_name )
    else _mk_FileNamePair (input_name + tc .soda_extension) (input_name + tc .scala_extension)

  private def _translate_append_with_input (input : String) (output_file_name : String) : Boolean =
    writer .append_file (output_file_name) (content = tc .new_line +
      tc .translator .translate (input) + tc .new_line)

  def translate_append (input_file_name : String) (output_file_name : String) : Boolean =
    _translate_append_with_input (reader .read_file (input_file_name) ) (output_file_name)

  def process_soda_file_with (pair : FileNamePair) : Boolean =
    translate_append (pair .input_file_name) (pair .output_file_name)

  def process_soda_file (file : File) : Boolean =
    process_soda_file_with (
      get_input_output_file_names (file .getAbsolutePath) (file .getParent) )

}

case class PackageProcessor_ () extends PackageProcessor

/**
 * This translates Soda source code to Scala source code.
 */

trait TranslatorToScala
  extends
    soda.translator.extension.common.Extension
{

  import   soda.translator.io.DirectoryProcessor_
  import   soda.translator.io.SimpleFileReader_
  import   soda.translator.io.SimpleFileWriter_
  import   java.io.File

  lazy val tc = TranslatorToScalaConstant_ ()

  lazy val reader = SimpleFileReader_ ()

  lazy val writer = SimpleFileWriter_ ()

  def process_soda_file_with_package_option (file : File) : Boolean =
    if ( file .getName == tc .package_file_name
    ) IndividualProcessor_ () .process_soda_file (file)
    else PackageProcessor_ () .process_soda_file (file)

  private def _process_directory_with_package_option (start : String) : Boolean =
    DirectoryProcessor_ (start , process_soda_file_with_package_option) .process ()

  private def _process_directory_with_single_files_option (start : String) : Boolean =
    DirectoryProcessor_ (start , IndividualProcessor_ () .process_soda_file) .process ()

  private def _is_single_files_option (s : String) : Boolean =
    (s == tc .single_files_option_1) || (s == tc .single_files_option_2)

  def execute_for (arguments : Seq [String] ) : Boolean =
    arguments.length match  {
      case 0 => _process_directory_with_package_option (tc .default_argument)
      case 1 => _process_directory_with_package_option (arguments .apply (0) )
      case 2 =>
        if ( _is_single_files_option (arguments .apply (0) )
        ) _process_directory_with_single_files_option (arguments .apply (1) )
        else false
      case 3 =>
        if ( _is_single_files_option (arguments .apply (0) )
        ) IndividualProcessor_ () .translate (arguments .apply (1) ) (arguments .apply (2) )
        else false
      case otherwise => false
    }

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      execute_for (arguments)

}

case class TranslatorToScala_ () extends TranslatorToScala


trait TypeParameterBlockTranslator
  extends
    soda.translator.blocktr.TokenizedBlockTranslator
{

  import   soda.translator.parser.SodaConstant_
  import   soda.translator.replacement.Token

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  lazy val replace_token : Token => String =
     token =>
      token
        .text
        .replaceAll (_sc .type_parameter_separation_regex ,
          _tc .scala_type_parameter_separator_symbol + _tc .scala_space)

}

case class TypeParameterBlockTranslator_ () extends TypeParameterBlockTranslator

