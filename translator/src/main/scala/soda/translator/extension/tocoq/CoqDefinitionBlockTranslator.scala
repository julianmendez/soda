package soda.translator.extension.tocoq

/*
 * This package contains classes for the translation to Gallina, the specification language used by Coq.
 */





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

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case FunctionDefinitionAnnotation_ (block) => _translate_definition_block (FunctionDefinitionAnnotation_ (block) )
      case x => annotated_block
    }

  private def _translate_definition_block (block : FunctionDefinitionAnnotation) : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation_ (_translate_block (block) )

  private def _translate_block (block : FunctionDefinitionAnnotation) : Block =
    if ( is_a_recursive_definition (block)
    ) _append (_tc.coq_recursive_definition_end_symbol) (_prepend (_tc.coq_recursive_definition_reserved_word + _tc.coq_space) (block) )
    else _translate_non_recursive_definition (block)

  private def _translate_non_recursive_definition (block : FunctionDefinitionAnnotation) : Block =
    if ( is_a_definition (block)
    ) _append (_tc.coq_definition_end_symbol) (_prepend (_tc.coq_definition_reserved_word + _tc.coq_space) (block) )
    else block

  private def _prepend (prefix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      Seq[String] (prefix + block.lines.head) ++ block.lines.tail
    )

  private def _append (suffix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      block.lines.:+ (suffix)
    )

  def is_a_recursive_definition (block : Block) : Boolean =
    _tc.coq_recursive_function_prefixes.exists (  prefix => first_line (block).startsWith (prefix) )

  def first_line (block : Block) : String =
    block.lines.headOption.getOrElse ("").trim

  def is_a_definition (block : Block) : Boolean =
    ! is_a_recursive_definition (block) &&
    ! _tc.non_definition_block_prefixes.exists (  prefix => block.contents.trim.startsWith (prefix) )

}

case class CoqDefinitionBlockTranslator_ () extends CoqDefinitionBlockTranslator
