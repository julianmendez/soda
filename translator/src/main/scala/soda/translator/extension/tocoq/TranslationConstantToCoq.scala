package soda.translator.extension.tocoq

/**
 * This class contains constants that are specific for the Soda translator, like reserved words for Soda and Coq.
 */

trait TranslationConstantToCoq
{

  import   soda.translator.parser.SodaConstant_

  lazy val soda_constant = SodaConstant_ ()

  lazy val coq_space = " "

  lazy val coq_function_definition_symbol = ":="

  lazy val coq_subtype_symbol = "<:"

  lazy val coq_supertype_symbol = ">:"

  lazy val coq_function_arrow_symbol = "->"

  lazy val coq_empty_string = ""

  lazy val coq_lambda_reserved_word = "fun"

  lazy val coq_lambda_arrow_symbol = "=>"

  lazy val coq_case_arrow_symbol = "=>"

  lazy val coq_not_reserved_word = "notb"

  lazy val coq_and_reserved_word = "andb"

  lazy val coq_or_reserved_word = "orb"

  lazy val coq_definition : String = "Definition"

  lazy val coq_value : String = "Definition"

  lazy val coq_recursive_definition : String = "Fixpoint"

  lazy val coq_definition_end : String = "."

  lazy val coq_recursive_definition_end : String = "."

  lazy val coq_with_reserved_word : String = "with"

  lazy val coq_theorem_begin_reserved_word = "Theorem"

  lazy val coq_theorem_end = "."

  lazy val coq_proof_begin_reserved_word = "Proof."

  lazy val coq_proof_end_reserved_word = "Qed."

  lazy val coq_recursive_function_prefixes : Seq [String] =
    Seq (
      "rec_",
      "_rec_",
      "tailrec_",
      "_tailrec_",
      "@tailrec"
    )

  lazy val non_definition_block_prefixes : Seq [String] =
    Seq (
      soda_constant.package_reserved_word,
      soda_constant.import_reserved_word,
      soda_constant.class_end_reserved_word,
      soda_constant.class_reserved_word,
      soda_constant.comment_opening_symbol
    )

  lazy val coq_reserved_words =
    coq_1 ++ coq_2 ++ coq_3 ++ coq_4

  lazy val coq_1 : Seq [String] =
    Seq (
      "as",
      "else",
      "end",
      "forall",
      "fun",
      "if",
      "in",
      "let",
      "match",
      "then",
      "with"
    )

  lazy val coq_2 : Seq [String] =
    Seq (
      "Admitted",
      "Arguments",
      "Check",
      "Constructors",
      "End",
      "Eval",
      "Export",
      "Hint",
      "Implicit",
      "Import",
      "Module",
      "Notation",
      "Print",
      "Proof",
      "Qed",
      "Require",
      "Resolve",
      "Section",
      "Set",
      "Type",
      "Unset"
    )

  lazy val coq_3 : Seq [String] =
    Seq (
      "admit",
      "apply",
      "assert",
      "auto",
      "case",
      "compute",
      "destruct",
      "discriminate",
      "elim",
      "exact",
      "induction",
      "intros",
      "pose",
      "refine",
      "rewrite",
      "simpl",
      "specialize",
      "unfold"
    )

  lazy val coq_4 : Seq [String] =
    Seq (
      "CoFixpoint",
      "CoInductive",
      "Definition",
      "Example",
      "Fixpoint",
      "Global",
      "Hypothesis",
      "Inductive",
      "Instance",
      "Lemma",
      "Ltac",
      "Parameter",
      "Theorem",
      "Variable"
    )

  lazy val type_symbols_translation : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (soda_constant.subtype_reserved_word, coq_subtype_symbol),
      Tuple2 (soda_constant.supertype_reserved_word, coq_supertype_symbol),
      Tuple2 (soda_constant.function_arrow_symbol, coq_function_arrow_symbol)
    )

  lazy val function_symbols_translation : Seq [Tuple2 [String, String] ] =
    Seq (
      Tuple2 (soda_constant.function_definition_symbol, coq_function_definition_symbol),
      Tuple2 (soda_constant.lambda_reserved_word, coq_lambda_reserved_word),
      Tuple2 (soda_constant.lambda_arrow_symbol, coq_lambda_arrow_symbol),
      Tuple2 (soda_constant.case_arrow_symbol, coq_case_arrow_symbol),
      Tuple2 (soda_constant.not_reserved_word, coq_not_reserved_word ),
      Tuple2 (soda_constant.and_reserved_word, coq_and_reserved_word ),
      Tuple2 (soda_constant.or_reserved_word, coq_or_reserved_word )
    )

  lazy val type_translation : Seq [ Tuple2 [String, String]  ] =
    Seq (
        Tuple2 ("Boolean", "bool"),
        Tuple2 ("Nat", "nat"),
        Tuple2 ("Option", "option"),
        Tuple2 ("List", "list"),
        Tuple2 ("String", "string"),
        Tuple2 ("BigInt", "Z")
    )

  lazy val prefix_coq_non_soda = "__soda__"

  lazy val coq_non_soda : Seq [Tuple2 [String, String] ] =
    coq_reserved_words
      .filter (  x => ! soda_constant.soda_reserved_words.contains (x))
      .map (  x => Tuple2 (x, prefix_coq_non_soda + x) )

  def is_coq_word (word : String) : Boolean =
    coq_reserved_words.contains (word)

  def is_soda_word (word : String) : Boolean =
    soda_constant.soda_reserved_words.contains (word)

}

case class TranslationConstantToCoq_ () extends TranslationConstantToCoq
