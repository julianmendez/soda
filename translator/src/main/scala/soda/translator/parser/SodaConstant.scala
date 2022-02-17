package soda.translator.parser

trait SodaConstant
{

  lazy val space = " "

  lazy val new_line = "\n"

  lazy val function_definition_symbol = "="

  lazy val type_membership_symbol = ":"

  lazy val function_arrow_symbol = "->"

  lazy val lambda_arrow_symbol = "-->"

  lazy val case_arrow_symbol = "==>"

  lazy val parameter_definition_symbol = ":="

  lazy val lambda_reserved_word = "lambda"

  lazy val if_reserved_word = "if"

  lazy val then_reserved_word = "then"

  lazy val else_reserved_word = "else"

  lazy val match_reserved_word = "match"

  lazy val case_reserved_word = "case"

  lazy val match_end_reserved_word = "end"

  lazy val class_reserved_word = "class"

  lazy val extends_reserved_word = "extends"

  lazy val abstract_reserved_word = "abstract"

  lazy val class_end_reserved_word = "end"

  lazy val this_reserved_word = "this"

  lazy val subtype_reserved_word = "subtype"

  lazy val supertype_reserved_word = "supertype"

  lazy val false_reserved_word = "false"

  lazy val true_reserved_word = "true"

  lazy val not_reserved_word = "not"

  lazy val and_reserved_word = "and"

  lazy val or_reserved_word = "or"

  lazy val package_reserved_word = "package"

  lazy val import_reserved_word = "import"

  lazy val theorem_reserved_word = "theorem"

  lazy val proof_reserved_word = "proof"

  lazy val constructor_suffix = "_"

  lazy val function_definition_synonym = "is"

  lazy val test_special_function = "test"

  lazy val subtype_abbreviation = "<:"

  lazy val supertype_abbreviation =  ">:"

  lazy val opening_parenthesis_symbol = "("

  lazy val closing_parenthesis_symbol = ")"

  lazy val opening_bracket_symbol = "["

  lazy val closing_bracket_symbol = "]"

  lazy val parameter_separator_symbol = ","

  lazy val addition_symbol = "+"

  lazy val subtraction_symbol = "-"

  lazy val multiplication_symbol = "*"

  lazy val division_symbol = "/"

  lazy val modulo_symbol = "%"

  lazy val case_abbreviation = "|"

  lazy val comment_opening_symbol = "/*"

  lazy val comment_closing_symbol = "*/"

  lazy val tail_recursion_annotation = "@tailrec"

  lazy val override_annotation = "@override"

  lazy val new_annotation = "@new"

  lazy val deprecated_let_reserved_word = "let"

  lazy val deprecated_in_reserved_word = "in"

  lazy val deprecated_has_reserved_word = "has"

  lazy val deprecated_with_reserved_word = "with"

  lazy val deprecated_class_abbreviation = "*"

  lazy val deprecated_class_beginning_symbol = "{"

  lazy val deprecated_class_end_symbol = "}"

  lazy val deprecated_class_definition_symbol = "="

  lazy val deprecated_class_definition_synonym = "is"

  lazy val deprecated_import_abbreviation = "+"

  lazy val soda_reserved_words = Seq (
    function_definition_symbol,
    type_membership_symbol,
    function_arrow_symbol,
    case_arrow_symbol,
    parameter_definition_symbol,
    lambda_reserved_word,
    if_reserved_word,
    then_reserved_word,
    else_reserved_word,
    match_reserved_word,
    case_reserved_word,
    class_reserved_word,
    extends_reserved_word,
    abstract_reserved_word,
    class_end_reserved_word,
    this_reserved_word,
    subtype_reserved_word,
    supertype_reserved_word,
    false_reserved_word,
    true_reserved_word,
    not_reserved_word,
    and_reserved_word,
    or_reserved_word,
    package_reserved_word,
    import_reserved_word,
    theorem_reserved_word,
    proof_reserved_word,
    addition_symbol,
    subtraction_symbol,
    multiplication_symbol,
    division_symbol,
    modulo_symbol,
    function_definition_synonym,
    subtype_abbreviation,
    supertype_abbreviation,
    case_abbreviation,
    deprecated_let_reserved_word,
    deprecated_in_reserved_word,
    deprecated_has_reserved_word,
    deprecated_with_reserved_word
  )

}

case class SodaConstant_ () extends SodaConstant
