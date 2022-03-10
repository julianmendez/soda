package soda.translator.extension.tocoq

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

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case PackageDeclarationAnnotation_ (block) => _translate_block (PackageDeclarationAnnotation_ (block) )
      case x => annotated_block
    }

  private def _translate_block (block : PackageDeclarationAnnotation) : PackageDeclarationAnnotation =
    PackageDeclarationAnnotation_ (
      _comment_block (
        block
      )
    )

  private def _comment_block (block : Block) : Block =
    BlockBuilder_ ().build (
      ( (Seq (_tc.coq_opening_comment).++ (block.lines) ).++ (Seq (_tc.coq_closing_comment) ) ).++ (_tc.coq_prelude)
    )

}

case class CoqPackageDeclarationBlockTranslator_ () extends CoqPackageDeclarationBlockTranslator
