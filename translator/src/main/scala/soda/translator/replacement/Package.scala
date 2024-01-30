package soda.translator.replacement

/*
 * This package contains auxiliary classes for string manipulation,
 * especially related to replacement.
 */



trait Package

/**
 * This is to classify characters.
 */

trait CharType
  extends
    soda.lib.EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class CharType_ (ordinal : Int, name : String) extends CharType

object CharType {
  def mk (ordinal : Int) (name : String) : CharType =
    CharType_ (ordinal, name)
}

/**
 * This is an enumeration for all types of characters.
 */

trait CharTypeEnum
  extends
    soda.lib.Enum [CharType]
{



  private def _mk_CharType (ordinal : Int) (name : String) : CharType =
    CharType_ (ordinal, name)

  lazy val undefined_type = _mk_CharType (0) ("undefined_type")

  lazy val quotes_type = _mk_CharType (1) ("quotes_type")

  lazy val apostrophe_type = _mk_CharType (2) ("apostrophe_type")

  lazy val backslash_type = _mk_CharType (3) ("backslash_type")

  lazy val plain_type = _mk_CharType (4) ("plain_type")

  lazy val values = Seq (undefined_type , quotes_type , apostrophe_type , backslash_type , plain_type)

  lazy val backslash_char = '\\'

  lazy val quotes_char = '\"'

  lazy val apostrophe_char = '\''

  lazy val underscore_char = '_'

  lazy val last_standard_symbol : Char = 0x7F

  lazy val symbol_chars : Seq [Char] =
    Seq ('!' , '#' , '$' , '%' , '&' , '(' , ')' , '*' , '+' , ',' , '-' , '.' , '/' , ':' ,
      ';' , '<' , '=' , '>' , '?' , '@' , '[' , ']' , '^' , '`' , '{' , '|' , '}' , '~')

  private def _is_plain (ch : Char) : Boolean =
    _is_whitespace (ch) || _is_letter_or_digit_or_underscore (ch) || _is_symbol (ch) ||
      _is_extended_character (ch)

  private def _is_whitespace (ch : Char) : Boolean =
    ch.isWhitespace

  private def _is_letter_or_digit_or_underscore (ch : Char) : Boolean =
    ch.isLetterOrDigit || ch == underscore_char

  private def _is_symbol (ch : Char) : Boolean =
    symbol_chars.contains (ch)

  private def _is_extended_character (ch: Char): Boolean =
    ch > last_standard_symbol

  private def _get_char_type_if_plain (ch : Char) : CharType =
    if ( _is_plain (ch)
    ) plain_type
    else undefined_type

  lazy val simple_char_type_map : Map [Char, CharType] =
    Map (
      Tuple2 (quotes_char , quotes_type) ,
      Tuple2 (apostrophe_char , apostrophe_type) ,
      Tuple2 (backslash_char , backslash_type)
    )

  def get_char_type (ch : Char) : CharType =
    simple_char_type_map
      .getOrElse (ch , _get_char_type_if_plain (ch) )

}

case class CharTypeEnum_ () extends CharTypeEnum

object CharTypeEnum {
  def mk : CharTypeEnum =
    CharTypeEnum_ ()
}


/**
 * This models all the possible states that the parser can be.
 */

trait ParserState
  extends
    soda.lib.EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class ParserState_ (ordinal : Int, name : String) extends ParserState

object ParserState {
  def mk (ordinal : Int) (name : String) : ParserState =
    ParserState_ (ordinal, name)
}

/**
 * This is an enumeration of all the parser states.
 */

trait ParserStateEnum
  extends
    soda.lib.Enum [ParserState]
{



  lazy val undefined_state = ParserState_ (0 , "undefined_state")

  lazy val quotes_state = ParserState_ (1 , "quotes_state")

  lazy val apostrophe_state = ParserState_ (2 , "apostrophe_state")

  lazy val quotes_backslash_state = ParserState_ (3 , "quotes_backslash_state")

  lazy val apostrophe_backslash_state = ParserState_ (4 , "apostrophe_backslash_state")

  lazy val plain = ParserState_ (5 , "plain")

  lazy val values = Seq (undefined_state , quotes_state , apostrophe_state , quotes_backslash_state ,
    apostrophe_backslash_state , plain)

  private def _is_like (x : ParserState) (y : ParserState) : Boolean =
     (x == quotes_state && y == quotes_backslash_state) ||
       (x == apostrophe_state && y == apostrophe_backslash_state)

  def is_same_class (x : ParserState) (y : ParserState) : Boolean =
     (x == y) || _is_like (x) (y) || _is_like (y) (x)

}

case class ParserStateEnum_ () extends ParserStateEnum

object ParserStateEnum {
  def mk : ParserStateEnum =
    ParserStateEnum_ ()
}

trait ParserTransition
{



  lazy val ps = ParserStateEnum_ ()

  lazy val ct = CharTypeEnum_ ()

  lazy val transitions_that_change_states : Map [Tuple2 [ParserState, CharType] , ParserState] =
    Map (
      /* */
      Tuple2 (Tuple2 (ps .quotes_state , ct .undefined_type) , ps .undefined_state),
      Tuple2 (Tuple2 (ps .quotes_state , ct .quotes_type) , ps .plain),
      Tuple2 (Tuple2 (ps .quotes_state , ct .backslash_type) , ps .quotes_backslash_state),
      /* */
      Tuple2 (Tuple2 (ps .apostrophe_state , ct .undefined_type) , ps .undefined_state),
      Tuple2 (Tuple2 (ps .apostrophe_state , ct .apostrophe_type) , ps .plain),
      Tuple2 (Tuple2 (ps .apostrophe_state , ct .backslash_type) ,
        ps .apostrophe_backslash_state),
      /* */
      Tuple2 (Tuple2 (ps .quotes_backslash_state , ct .undefined_type) , ps .undefined_state),
      Tuple2 (Tuple2 (ps .quotes_backslash_state , ct .quotes_type) , ps .quotes_state),
      Tuple2 (Tuple2 (ps .quotes_backslash_state , ct .apostrophe_type) , ps .quotes_state),
      Tuple2 (Tuple2 (ps .quotes_backslash_state , ct .backslash_type) , ps .quotes_state),
      Tuple2 (Tuple2 (ps .quotes_backslash_state , ct .plain_type) , ps .quotes_state),
      /* */
      Tuple2 (Tuple2 (ps .apostrophe_backslash_state , ct .undefined_type) ,
        ps .undefined_state),
      Tuple2 (Tuple2 (ps .apostrophe_backslash_state , ct .quotes_type) ,
        ps .apostrophe_state),
      Tuple2 (Tuple2 (ps .apostrophe_backslash_state , ct .apostrophe_type) ,
        ps .apostrophe_state),
      Tuple2 (Tuple2 (ps .apostrophe_backslash_state , ct .backslash_type) ,
        ps .apostrophe_state),
      Tuple2 (Tuple2 (ps .apostrophe_backslash_state , ct .plain_type) ,
        ps .apostrophe_state),
      /* */
      Tuple2 (Tuple2 (ps .plain , ct .undefined_type) , ps .undefined_state),
      Tuple2 (Tuple2 (ps .plain , ct .quotes_type) , ps .quotes_state),
      Tuple2 (Tuple2 (ps .plain , ct .apostrophe_type) , ps .apostrophe_state),
      Tuple2 (Tuple2 (ps .plain , ct .backslash_type) , ps .plain),
      Tuple2 (Tuple2 (ps .plain , ct .plain_type) , ps .plain)
    )

  def next_parser_state (parser_state : ParserState) (char_type : CharType) : ParserState =
    transitions_that_change_states .getOrElse (Tuple2 (parser_state , char_type) , parser_state)

}

case class ParserTransition_ () extends ParserTransition

object ParserTransition {
  def mk : ParserTransition =
    ParserTransition_ ()
}


trait ReplacementAux
{



  lazy val soda_space = " "

  lazy val scala_space = " "

  def replace_at (index : Int) (line : String) (pattern : String) (replacement : String) : String =
    if ( (0 <= index) && (index + pattern .length <= line .length)
    ) line .substring (0 , index) + replacement + line .substring (index + pattern .length ,
      line .length)
    else line

  def replace_first (line : String) (pattern : String) (replacement : String) : String =
    replace_at (line .indexOf (pattern) ) (line) (pattern) (replacement)

  def replace_if_found_at_beginning (line : String) (pattern : String) (new_text : String)
      : String =
    if ( line .trim .startsWith (pattern .trim)
    ) replace_first (line) (pattern) (new_text)
    else line

  def replace_all (line : String) (pattern : String) (replacement : String) : String =
    Replacer_ (line , pattern , replacement) .replaced_text

  def replace_if_found (line : String) (pattern : String) (new_text : String) : String =
    if ( line .contains (pattern)
    ) replace_all (line) (pattern) (new_text)
    else line

  private def _left_part_of_symbols (line : String) (symbols : Set [Char] ) (index : Int) (ch : Char)
      : String =
    if ( (index > 0) && symbols .contains (ch) &&
      ! line (index - 1) .isWhitespace
    ) scala_space
    else ""

  private def _right_part_of_symbols (line : String) (symbols : Set [Char] ) (index : Int) (ch : Char)
      : String =
    if ( (index < line .length - 1) && symbols .contains (ch) &&
      ! line (index + 1) .isWhitespace
    ) scala_space
    else ""

  private def _add_spaces_to_symbols_with (ch : Char) (index : Int) (line : String) (symbols : Set [Char] )
      : String =
    (_left_part_of_symbols (line) (symbols) (index) (ch) ) + ch + (
      _right_part_of_symbols (line) (symbols) (index) (ch) )

  def add_spaces_to_symbols (line : String) (symbols : Set [Char] ) : String =
    line
      .indices
      .map ( index =>
        _add_spaces_to_symbols_with (line (index) ) (index ) (line) (symbols) )
      .mkString ("")

  private def _get_line_without_starting_space (line : String) : String =
    if ( line .startsWith (scala_space)
    ) line .substring (1)
    else line

  private def _get_line_without_ending_space (line : String) : String =
    if ( line .endsWith (scala_space)
    ) line .substring (0 , line .length - 1)
    else line

  def remove_space_from_translated_line (line : String) : String =
    _get_line_without_ending_space (_get_line_without_starting_space (line) )

  private def _add_after_spaces_or_pattern_with (prefix_length : Int) (line : String) (pattern : String)
      (text_to_prepend : String) : String =
    line .substring (0 , prefix_length) + text_to_prepend + line .substring (prefix_length)

  private def _get_prefix_length (line : String) (pattern : String) : Int =
    if ( line .trim .startsWith (pattern)
    ) line .indexOf (pattern) + pattern .length
    else line .takeWhile ( ch => ch .isSpaceChar) .length

  def add_after_spaces_or_pattern (line : String) (pattern : String) (text_to_prepend : String)
      : String =
    _add_after_spaces_or_pattern_with (
      _get_prefix_length (line) (pattern) ) (line) (pattern) (text_to_prepend)

}

case class ReplacementAux_ () extends ReplacementAux

object ReplacementAux {
  def mk : ReplacementAux =
    ReplacementAux_ ()
}

trait ReplacementWithTranslator
{

  def   translator : soda.translator.block.Translator

  import   soda.lib.Fold_

  lazy val aux = ReplacementAux_ ()

  lazy val soda_space = aux .soda_space

  lazy val scala_space = aux .scala_space

  lazy val opening_parenthesis_symbol = "("

  lazy val closing_parenthesis_symbol = ")"

  lazy val new_line = "\n"

  lazy val scala_opening_parenthesis_symbol = "("

  private lazy val _fold = Fold_ ()

  private def _next_replace_words_with (line : String) (reserved_word : String) (translation : String)
      : String =
    Replacement_ (line)
      .replace_if_found (soda_space) (reserved_word) (soda_space) (translation)
      .replace_if_found (soda_space) (reserved_word) (closing_parenthesis_symbol) (translation)
      .replace_if_found (soda_space) (reserved_word) (new_line) (translation)
      .replace_if_found (opening_parenthesis_symbol) (reserved_word) (soda_space) (translation)
      .replace_if_found (opening_parenthesis_symbol) (reserved_word) (
        closing_parenthesis_symbol) (translation)
      .replace_if_found (opening_parenthesis_symbol) (reserved_word) (new_line) (translation)
      .line

  private def _next_replace_words (line : String) (reserved_word : String) : String =
    _next_replace_words_with (line) (reserved_word) (translator .translate (reserved_word) )

  def replace_words (line : String) : String =
    _fold .apply [String, String] (translator .keys) (line) (_next_replace_words)

  private def _next_replace_symbols (line : String) (reserved_word : String) : String =
    aux .replace_if_found (line) (reserved_word) (translator .translate (reserved_word) )

  def replace_symbols (line : String) : String =
    _fold .apply [String, String] (translator .keys) (line) (_next_replace_symbols)

  private def _next_replace_only_beginning (line : String) (reserved_word : String) : String =
    aux .replace_if_found_at_beginning (line) (
      soda_space + reserved_word + soda_space) (scala_space +
        translator .translate (reserved_word) + scala_space)

  private def _replace_only_beginning (line : String) : String =
    _fold .apply [String, String] (translator .keys) (line) (_next_replace_only_beginning)

  def replace_at_beginning (line : String) (index : Int) : String =
    if ( index == 0
    ) _replace_only_beginning (line)
    else line

}

case class ReplacementWithTranslator_ (translator : soda.translator.block.Translator) extends ReplacementWithTranslator

object ReplacementWithTranslator {
  def mk (translator : soda.translator.block.Translator) : ReplacementWithTranslator =
    ReplacementWithTranslator_ (translator)
}

/**
 * This models a collection of replacement functions.
 * This is intended to be used as a pipeline.
 */

trait Replacement
  extends
    soda.translator.block.SingleLineProcessor
{

  def   line : String

  import   soda.translator.block.Translator

  lazy val aux = ReplacementAux_ ()

  lazy val soda_space = aux.soda_space

  def replace_at_beginning (index : Int) (translator : Translator) : Replacement =
    Replacement_ (ReplacementWithTranslator_ (translator) .replace_at_beginning (line) (index) )

  def replace_all (pattern : String) (replacement : String) : Replacement =
    Replacement_ (aux .replace_all (line) (pattern) (replacement) )

  def add_space_to_soda_line () : Replacement =
    Replacement_ (soda_space + line + soda_space)

  def remove_space_from_translated_line () : Replacement =
    Replacement_ (aux .remove_space_from_translated_line (line) )

  def add_after_spaces_or_pattern (pattern : String) (text_to_prepend : String) : Replacement =
    Replacement_ (aux .add_after_spaces_or_pattern (line) (pattern) (text_to_prepend) )

  def replace_if_found  (previous_character : String) (text : String)  (next_character : String)
     (replacement : String) : Replacement =
    Replacement_ (
      aux .replace_if_found (line) (
        previous_character + text + next_character) (
        previous_character + replacement + next_character)
    )

}

case class Replacement_ (line : String) extends Replacement

object Replacement {
  def mk (line : String) : Replacement =
    Replacement_ (line)
}


trait ReplacerFoldTuple
{

  def   replaced_text_rev : Seq [String]
  def   start_index : Int

}

case class ReplacerFoldTuple_ (replaced_text_rev : Seq [String], start_index : Int) extends ReplacerFoldTuple

object ReplacerFoldTuple {
  def mk (replaced_text_rev : Seq [String]) (start_index : Int) : ReplacerFoldTuple =
    ReplacerFoldTuple_ (replaced_text_rev, start_index)
}

trait LinePatternProcessor
{

  def   line : String
  def   pattern : String
  def   replacement : String

}

case class LinePatternProcessor_ (line : String, pattern : String, replacement : String) extends LinePatternProcessor

object LinePatternProcessor {
  def mk (line : String) (pattern : String) (replacement : String) : LinePatternProcessor =
    LinePatternProcessor_ (line, pattern, replacement)
}

trait Replacer
  extends
    LinePatternProcessor
{

  def   line : String
  def   pattern : String
  def   replacement : String

  import   soda.lib.FoldWhile_
  import   soda.lib.Range_

  private lazy val _fold_while = FoldWhile_ ()

  private lazy val _range = Range_ ()

  lazy val initial_value : ReplacerFoldTuple = ReplacerFoldTuple_ (Seq () , 0 )

  private def _get_next_tuple (replaced_text_rev : Seq [String] ) (start_index : Int) (pos : Int)
      : ReplacerFoldTuple =
    if ( pos == -1
    ) ReplacerFoldTuple_ (replaced_text_rev .+: (line .substring (start_index) ) , pos )
    else
      ReplacerFoldTuple_ (
        (replaced_text_rev .+: (line .substring (start_index , pos) ) ) .+: (replacement) ,
        pos + pattern .length
      )

  def next_value_function (tuple : ReplacerFoldTuple) (x : Int) : ReplacerFoldTuple =
    _get_next_tuple (
      replaced_text_rev = tuple .replaced_text_rev) (
      start_index = tuple .start_index) (
      pos = line .indexOf (pattern , tuple .start_index)
    )

  def should_continue (tuple : ReplacerFoldTuple) (x : Int) : Boolean =
    ! (tuple .start_index == -1)

  def postprocess (tuple : ReplacerFoldTuple) : String =
    tuple .replaced_text_rev .reverse .mkString ("")

  lazy val replaced_text =
    postprocess (_fold_while .apply [Int, ReplacerFoldTuple] (
      _range .apply (line .length) ) (initial_value) (next_value_function) (should_continue) )

}

case class Replacer_ (line : String, pattern : String, replacement : String) extends Replacer

object Replacer {
  def mk (line : String) (pattern : String) (replacement : String) : Replacer =
    Replacer_ (line, pattern, replacement)
}


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

object Token {
  def mk (text : String) (parser_state : ParserState) (index : Int) : Token =
    Token_ (text, parser_state, index)
}

trait TokenizerFoldTuple
{

  def   last_index : Int
  def   parser_state : ParserState
  def   rev_tokens : Seq [Token]

}

case class TokenizerFoldTuple_ (last_index : Int, parser_state : ParserState, rev_tokens : Seq [Token]) extends TokenizerFoldTuple

object TokenizerFoldTuple {
  def mk (last_index : Int) (parser_state : ParserState) (rev_tokens : Seq [Token]) : TokenizerFoldTuple =
    TokenizerFoldTuple_ (last_index, parser_state, rev_tokens)
}

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

  private def _mk_Token (text : String) (parser_state : ParserState) (index : Int) : Token =
    Token_ (text, parser_state, index)

  private def _mk_TokenizerFoldTuple (last_index : Int) (parser_state : ParserState)
      (rev_tokens : Seq [Token] ) : TokenizerFoldTuple =
    TokenizerFoldTuple_ (last_index, parser_state, rev_tokens)

  private lazy val _fold = Fold_ ()

  private lazy val _range = Range_ ()

  private def _new_parser_state (tuple : TokenizerFoldTuple) (current_index : Int) : ParserState =
    ParserTransition_ ()
      .next_parser_state (
        tuple .parser_state) (
        CharTypeEnum_ () .get_char_type (line .charAt (current_index) )
      )

  private def _get_new_current_index (tuple : TokenizerFoldTuple) (current_index : Int) : Int =
    if ( tuple .parser_state == ParserStateEnum_ () .quotes_state ||
     tuple .parser_state == ParserStateEnum_ () .apostrophe_state
    ) current_index + 1
    else current_index

  private def _next_value_function_of_different_class_with (tuple : TokenizerFoldTuple)
      (current_index : Int) (new_parser_state : ParserState) (index : Int)
      : TokenizerFoldTuple =
    _mk_TokenizerFoldTuple (index) (new_parser_state) (
      tuple .rev_tokens .+: (
        _mk_Token (
          line .substring (tuple .last_index, index) ) (
          tuple .parser_state ) (
          tuple .last_index
        )
      )
    )

  private def _next_value_function_of_different_class (tuple : TokenizerFoldTuple) (current_index : Int)
      (new_parser_state : ParserState) : TokenizerFoldTuple =
    _next_value_function_of_different_class_with (tuple) (current_index) (new_parser_state) (
      _get_new_current_index (tuple) (current_index)
    )

  private def _new_value_function_with (tuple : TokenizerFoldTuple) (current_index : Int)
      (new_parser_state : ParserState) : TokenizerFoldTuple =
    if ( ParserStateEnum_ () .is_same_class (new_parser_state) (tuple .parser_state)
    ) _mk_TokenizerFoldTuple (tuple .last_index) (new_parser_state) (tuple .rev_tokens)
    else _next_value_function_of_different_class (tuple) (current_index) (new_parser_state)

  private def _next_value_function (tuple : TokenizerFoldTuple) (current_index : Int) : TokenizerFoldTuple =
    _new_value_function_with (tuple) (current_index) (
      _new_parser_state (tuple) (current_index) )

  private def _postprocess (tuple : TokenizerFoldTuple) : Seq [Token] =
    (tuple .rev_tokens .+: (Token_ (line .substring (tuple .last_index) , tuple .parser_state ,
        tuple .last_index) ) )
      .reverse

  private lazy val _initial_value  : TokenizerFoldTuple =
    _mk_TokenizerFoldTuple (0) (ParserStateEnum_ () .plain) (Seq () )

  lazy val tokens : Seq [Token] =
    _postprocess (_fold .apply [Int, TokenizerFoldTuple] (
      _range .apply (line .length) ) (_initial_value) (_next_value_function) )

}

case class Tokenizer_ (line : String) extends Tokenizer

object Tokenizer {
  def mk (line : String) : Tokenizer =
    Tokenizer_ (line)
}

