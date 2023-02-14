package soda.translator.replacement

/*
 * This package contains auxiliary classes for string manipulation,
 * especially related to replacement.
 */





/**
 * A token is a piece of code, that can contain one or more words combined with symbols.
 */

trait Token
{

  def   text : String
  def   parser_state : ParserState
  def   index : Int

}

case class Token_ (text : String, parser_state : ParserState, index : Int) extends Token

/**
 * This class processes a line to divide it into tokens.
 */

trait Tokenizer
  extends
    soda.translator.block.SingleLineProcessor
{

  def   line : String

  import   soda.lib.Fold_
  import   soda.lib.Range_

  private lazy val _fold = Fold_ ()

  private lazy val _range = Range_ ()

  lazy val tokens : Seq [Token] =
    _postprocess (_fold.apply (_range.apply (line.length) ) (_initial_value) (_next_value_function) )

  private lazy val _initial_value  : TokenizerFoldTuple = TokenizerFoldTuple_ (0, ParserStateEnum_ ().plain, Seq () )

  private def _postprocess (tuple : TokenizerFoldTuple) : Seq [Token] =
    (tuple.rev_tokens.+: (Token_ (line.substring (tuple.last_index), tuple.parser_state, tuple.last_index) ) )
      .reverse

  private def _next_value_function (tuple : TokenizerFoldTuple) (current_index : Int) : TokenizerFoldTuple =
    _new_value_function_with (tuple) (current_index) (_new_parser_state (tuple) (current_index) )

  private def _new_value_function_with (tuple : TokenizerFoldTuple) (current_index : Int) (new_parser_state : ParserState) : TokenizerFoldTuple =
    if ( ParserStateEnum_ ().is_same_class (new_parser_state) (tuple.parser_state)
    ) TokenizerFoldTuple_ (tuple.last_index, new_parser_state, tuple.rev_tokens)
    else _next_value_function_of_different_class (tuple) (current_index) (new_parser_state)

  private def _new_parser_state (tuple : TokenizerFoldTuple) (current_index : Int) : ParserState =
    ParserTransition_ ()
      .next_parser_state (
        tuple.parser_state) (
        CharTypeEnum_ ().get_char_type (line.charAt (current_index) )
      )

  private def _next_value_function_of_different_class (tuple : TokenizerFoldTuple) (current_index : Int) (new_parser_state : ParserState) : TokenizerFoldTuple =
    _next_value_function_of_different_class_with (
      tuple) (
      current_index) (
      new_parser_state) (
      _get_new_current_index (tuple) (current_index)
    )

  private def _get_new_current_index (tuple : TokenizerFoldTuple) (current_index : Int) : Int =
    if ( tuple.parser_state == ParserStateEnum_ ().quotes_state ||
     tuple.parser_state == ParserStateEnum_ ().apostrophe_state
    ) current_index + 1
    else current_index

  private def _next_value_function_of_different_class_with (tuple : TokenizerFoldTuple) (current_index : Int) (new_parser_state : ParserState) (index : Int) : TokenizerFoldTuple =
    TokenizerFoldTuple_ (index, new_parser_state,
      tuple.rev_tokens.+: (
        Token_ (
          line.substring (tuple.last_index, index),
          tuple.parser_state,
          tuple.last_index
        )
      )
    )

}

case class Tokenizer_ (line : String) extends Tokenizer

trait TokenizerFoldTuple
{

  def   last_index : Int
  def   parser_state : ParserState
  def   rev_tokens : Seq [Token]

}

case class TokenizerFoldTuple_ (last_index : Int, parser_state : ParserState, rev_tokens : Seq [Token]) extends TokenizerFoldTuple
