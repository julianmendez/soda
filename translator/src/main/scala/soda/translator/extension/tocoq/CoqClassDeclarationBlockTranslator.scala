package soda.translator.extension.tocoq

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

  lazy val soda_space : String = _sc.space

  lazy val scala_space : String = " "

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassBeginningAnnotation_ (block) => _translate_class_beginning_block (ClassBeginningAnnotation_ (block) )
      case x => annotated_block
    }

  private def _translate_class_beginning_block (block : ClassBeginningAnnotation) : ClassBeginningAnnotation =
    ClassBeginningAnnotation_ (_translate_block (block) )

  private def _translate_block (block : AnnotatedBlock) : Block =
    if ( (has_condition_for_type_alias (get_first_line (block) ) )
    ) block
    else
      BlockBuilder_ ().build (
        _process_head (block) ++ _process_tail (block)
      )

  private def _process_head (block : Block) : Seq [String] =
    _process_head_with (get_first_line (block) ) (block)

  private def _process_head_with (line : String) (block : Block) : Seq [String] =
    Seq [String] (
      Replacement_ (_sc.space + line).replace_at_beginning (0) (get_table_translator (line) ).line.substring (_sc.space.length) + _tc.coq_space + _tc.coq_end_symbol
    )

  private def _process_tail (block : Block) : Seq [String] =
    _process_if_extends (remove_first_line (block) )

  private def _process_if_extends (block : Block) : Seq [String] =
    if ( (get_first_line (block).trim == _sc.extends_reserved_word)
    ) Seq [String] (get_initial_spaces (block) ).++ ( _process_after_extends (remove_first_line (block) ) )
    else block.lines

  def get_table_translator (line : String) : Translator =
    TableTranslator_ (
      Seq (Tuple2 (_sc.class_reserved_word, _tc.coq_module_reserved_word ) )
    )

  private def _process_after_extends (block : Block) : Seq [String] =
    if ( (get_first_line (block).trim.nonEmpty)
    ) block.lines.map (  line => _tc.coq_import_reserved_word + _tc.coq_space + line.trim + _tc.coq_space + _tc.coq_end_symbol)
    else Seq [String] ()

  def remove_first_line (block : Block) : Block =
    BlockBuilder_ ().build ( _remove_first_line (block.lines) )

  private def _remove_first_line (lines : Seq [String] ) : Seq [String] =
    if ( lines.isEmpty
    ) lines
    else lines.tail

  def get_first_line (block : Block) : String =
    block.lines.headOption.getOrElse ("")

  def get_initial_spaces (block : Block) : String =
    get_initial_spaces_for (get_first_line (block) )

  def get_initial_spaces_for (line : String) : String =
    line.takeWhile (  ch => ch.isSpaceChar)

  def contains_equals (line : String) : Boolean =
    line.trim.contains (_sc.function_definition_symbol)

  def has_condition_for_type_alias (line : String) : Boolean =
    contains_equals (line)

}

case class CoqClassDeclarationBlockTranslator_ () extends CoqClassDeclarationBlockTranslator
