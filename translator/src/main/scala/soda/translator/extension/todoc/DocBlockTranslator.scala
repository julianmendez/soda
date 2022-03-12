package soda.translator.extension.todoc

trait DocBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.CommentAnnotation
  import   soda.translator.parser.annotation.CommentAnnotation_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToDoc_ ()

  private lazy val _comment_line_prefix = _sc.comment_line_symbol + _sc.space

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case CommentAnnotation_ (block) => _translate_comment (CommentAnnotation_ (block) )
      case x => _translate_source_code (x)
    }

  private def _translate_comment (block : CommentAnnotation) : CommentAnnotation =
    CommentAnnotation_ (
      BlockBuilder_ ().build (
        _append (
          _tc.doc_closing_comment_translation) (
          _prepend (_tc.doc_opening_comment_translation) (
            _remove_comment_delimiter (
              _remove_comment_line_prefix (block.lines)
            )
          )
        )
      )
    )

  private def _translate_source_code (block : AnnotatedBlock) : CommentAnnotation =
    CommentAnnotation_ (
      block
    )

  private def _prepend (prefix : String) (content : Seq [String] ) : Seq [String] =
    if ( content.isEmpty
    ) Seq [String] (prefix)
    else Seq[String] (prefix + content.head).++ (content.tail)

  private def _append (suffix : String) (content : Seq [String] ) : Seq [String] =
    content.:+ (suffix)

  private def _remove_comment_delimiter (content : Seq [String] ) : Seq [String] =
    _remove_last_delimiter (
      _remove_first_delimiter (content)
    )

  private def _remove_first_delimiter (content : Seq [String] ) : Seq [String] =
    if ( content.isEmpty
    ) content
    else _prepend (
      _remove_prefix_in_line (_sc.documentation_comment_opening_symbol) (
        _remove_prefix_in_line (_sc.comment_opening_symbol) (content.head)
     )
    ) (content.tail)

  private def _remove_last_delimiter (content : Seq [String] ) : Seq [String] =
    ( _remove_last_delimiter_on_first_line (content.reverse) ).reverse

  private def _remove_last_delimiter_on_first_line (content : Seq [String] ) : Seq [String] =
    if ( content.isEmpty
    ) content
    else _prepend (_remove_suffix_in_line (_sc.comment_closing_symbol) (content.head) ) (content.tail)

  private def _remove_comment_line_prefix (content : Seq [String] ) : Seq [String] =
    content.map (  line => _remove_prefix_in_line (_comment_line_prefix) (line) )

  private def _remove_prefix_in_line (prefix : String) (line : String) : String =
    _remove_prefix_in_line_at (line.indexOf (prefix) ) (prefix) (line)

  private def _remove_prefix_in_line_at (index : Int) (prefix : String) (line : String) : String =
    if ( index >= 0
    ) line.substring (index + prefix.length)
    else line

  private def _remove_suffix_in_line (suffix : String) (line : String) : String =
    _remove_suffix_in_line_at (line.lastIndexOf (suffix) ) (line)

  private def _remove_suffix_in_line_at (index : Int) (line : String) : String =
    if ( index >= 0
    ) line.substring (0, index)
    else line

}

case class DocBlockTranslator_ () extends DocBlockTranslator
