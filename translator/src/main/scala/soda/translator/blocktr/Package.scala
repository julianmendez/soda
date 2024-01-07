package soda.translator.blocktr

/*
 * This package contains shared block translators.
 *
 * @see soda.translator.block.BlockTranslator
 */



trait Package

trait DirectiveBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  def   identifier : String
  def   opening_comment : String
  def   closing_comment : String

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.DirectiveBlockAnnotation
  import   soda.translator.parser.annotation.DirectiveBlockAnnotation_

  private def _get_first_or_empty (sequence : Seq [String] ) : String =
    sequence match  {
      case x :: xs => x
      case otherwise => ""
    }

  private def _remove_first_if_possible (sequence : Seq [String] ) : Seq [String] =
    sequence match  {
      case x :: xs => xs
      case otherwise => sequence
    }

  private def _comment_block_out (lines : Seq [String] ) : Seq [String] =
    Seq (opening_comment) .++ (lines .++ (Seq (closing_comment) ) )

  private def _line_contains (line : String) (pattern : String) : Boolean =
    line .indexOf (pattern) >= 0

  private def _directive_applies (lines : Seq [String] ) : Boolean =
    (_get_first_or_empty (lines) ) .contains (identifier)

  private def _translate_lines (lines : Seq [String] ) : Seq [String] =
    if ( _directive_applies (lines)
    ) _remove_first_if_possible (lines)
    else _comment_block_out (lines)

  private def _translate_block (block : DirectiveBlockAnnotation) : DirectiveBlockAnnotation =
    DirectiveBlockAnnotation_ (
      BlockBuilder_ () .build (
        _translate_lines (block .lines)
      )
    )

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case DirectiveBlockAnnotation_ (block) => _translate_block (DirectiveBlockAnnotation_ (block) )
      case otherwise => annotated_block
    }

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class DirectiveBlockTranslator_ (identifier : String, opening_comment : String, closing_comment : String) extends DirectiveBlockTranslator


trait Table
{

  def   table : Seq [Tuple2 [String, String] ]

}

case class Table_ (table : Seq [Tuple2 [String, String] ]) extends Table

trait TableTranslator
  extends
    Table
    with soda.translator.block.Translator
{

  def   table : Seq [Tuple2 [String, String] ]

  lazy val keys : Seq [String] = table .map ( pair => pair ._1)

  lazy val translate : String => String =
     word =>
      table
        .toMap
        .get (word)
        .getOrElse (word)

}

case class TableTranslator_ (table : Seq [Tuple2 [String, String] ]) extends TableTranslator


trait TokenReplacement
{

  import   soda.translator.replacement.ReplacementWithTranslator_

  def replace_words (table : Seq [Tuple2 [String, String] ] ) : TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (
       token =>
        ReplacementWithTranslator_ (TableTranslator_ (table) ) .replace_words (token .text)
    )

  def replace_symbols (table : Seq [Tuple2 [String, String] ] ) : TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (
       token =>
        ReplacementWithTranslator_ (TableTranslator_ (table) ) .replace_symbols (token .text)
    )

}

case class TokenReplacement_ () extends TokenReplacement


trait TokenizedBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  def   replace_token : soda.translator.replacement.Token => String

  import   soda.lib.SomeSD_
  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedLine
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.annotation.AnnotationFactory_
  import   soda.translator.replacement.ParserStateEnum_
  import   soda.translator.replacement.Replacement_
  import   soda.translator.replacement.Token
  import   soda.translator.replacement.Token_
  import   soda.translator.replacement.Tokenizer_

  private def _join_tokens (tokens : Seq [Token] ) : String =
    tokens
      .map ( token => token .text)
      .mkString ("")

  private def _get_token_translated_if_in_state (token : Token) : Token =
    if ( token .parser_state == ParserStateEnum_ () .plain
    ) Token_ (replace_token (token) , token .parser_state , token .index)
    else token

  private def _translate_line (tokens : Seq [Token] ) : Seq [Token] =
    tokens
      .map ( token => _get_token_translated_if_in_state (token) )

  private def _translate_non_comment (line : String) : String =
      SomeSD_ (line)
        .map ( x => Replacement_ (x) .add_space_to_soda_line () .line)
        .map ( x => Tokenizer_ (x) .tokens)
        .map ( x => _translate_line (x) )
        .map ( x => _join_tokens (x) )
        .map ( x => Replacement_ (x) .remove_space_from_scala_line () .line)
        .getOrElse ("")

  private def _translate_if_not_a_comment (annotated_line : AnnotatedLine) : String =
    if ( annotated_line .is_comment
    ) annotated_line .line
    else _translate_non_comment (annotated_line .line)

  def translate_for (block : AnnotatedBlock) : AnnotatedBlock =
    AnnotationFactory_ () .update_block (
      block) (
      BlockBuilder_ () .build (
        block
          .annotated_lines
          .map ( annotated_line => _translate_if_not_a_comment (annotated_line) )
      )
    )

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

}

case class TokenizedBlockTranslator_ (replace_token : soda.translator.replacement.Token => String) extends TokenizedBlockTranslator

