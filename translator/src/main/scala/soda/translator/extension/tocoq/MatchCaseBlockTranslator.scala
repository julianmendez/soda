package soda.translator.extension.tocoq

trait MatchCaseBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.FunctionDefinitionAnnotation
  import   soda.translator.parser.annotation.TestDeclarationAnnotation

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val _labels = BlockAnnotationEnum_ ()

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block )

  def translate_for (annotated_block: AnnotatedBlock ): AnnotatedBlock =
    annotated_block match  {
      case block: FunctionDefinitionAnnotation => _translate_block (block )
      case block: TestDeclarationAnnotation => _translate_block (block )
      case x => annotated_block
    }

  def _translate_block (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      block.lines
        .map (line => append_with_after_match (line ) ), block.block_annotation
    )

  def append_with_after_match (line: String ): String =
    if (line.trim () .startsWith (tc.soda_match_pattern )
    ) line + tc.space + tc.coq_with_reserved_word
    else line

}

case class MatchCaseBlockTranslator_ ()
  extends
    MatchCaseBlockTranslator
{

}
