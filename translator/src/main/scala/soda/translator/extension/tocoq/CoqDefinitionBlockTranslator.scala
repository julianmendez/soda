package soda.translator.extension.tocoq

trait CoqDefinitionBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation_

  lazy val space = " "

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case FunctionDefinitionAnnotation_ (block) => _translate_definition_block (FunctionDefinitionAnnotation_ (block) )
      case x => annotated_block
    }

  def _translate_definition_block (block : FunctionDefinitionAnnotation) : FunctionDefinitionAnnotation =
    FunctionDefinitionAnnotation_ (_translate_block (block) )

  def _translate_block (block : FunctionDefinitionAnnotation) : Block =
    if ( is_a_recursive_definition (block)
    ) append (tc.coq_recursive_definition_end) (prepend (tc.coq_recursive_definition + space) (block) )
    else
      if ( is_a_definition (block)
      ) append (tc.coq_definition_end) (prepend (tc.coq_definition + space) (block) )
      else block

  def prepend (prefix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      Seq[String] (prefix + block.lines.head) ++ block.lines.tail
    )

  def append (suffix : String) (block : Block) : Block =
    BlockBuilder_ ().build (
      block.lines.:+ (suffix)
    )

  def is_a_recursive_definition (block : Block) : Boolean =
    tc.coq_recursive_function_prefixes.exists (  prefix => first_line (block).startsWith (prefix))

  def first_line (block : Block) : String =
    block.lines.headOption.getOrElse ("").trim

  def is_a_definition (block : Block) : Boolean =
    ! is_a_recursive_definition (block) &&
    ! tc.non_definition_block_prefixes.exists (  prefix => block.contents.trim.startsWith (prefix))

}

case class CoqDefinitionBlockTranslator_ () extends CoqDefinitionBlockTranslator
