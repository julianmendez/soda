package soda.translator.extension.tocoq

/**
 * This class contains constants that are specific for the Soda translator, like reserved words for Soda and Coq.
 */

trait TranslationConstantToCoq
{

  import   soda.translator.parser.SodaConstant_

  lazy val soda_constant = SodaConstant_ ()

  lazy val soda_let_pattern = soda_constant.deprecated_let_reserved_word + " "

  lazy val soda_in_pattern = soda_constant.deprecated_in_reserved_word + " "

  lazy val soda_in_let_pattern = soda_constant.deprecated_in_reserved_word + " " + soda_constant.deprecated_let_reserved_word + " "

  lazy val coq_space = " "

  lazy val coq_definition: String = "Definition"

  lazy val coq_value: String = "Definition"

  lazy val coq_recursive_definition: String = "Fixpoint"

  lazy val coq_definition_end: String = "."

  lazy val coq_recursive_definition_end: String = "."

  lazy val coq_with_reserved_word: String = "with"

  lazy val coq_theorem_begin_reserved_word = "Theorem"

  lazy val coq_theorem_end = "."

  lazy val coq_proof_begin_reserved_word = "Proof."

  lazy val coq_proof_end_reserved_word = "Qed."

  lazy val coq_recursive_function_prefixes: Seq [String] =
    Seq (
      "rec_",
      "_rec_",
      "tailrec_",
      "_tailrec_",
      "@tailrec"
    )

  lazy val non_definition_block_prefixes: Seq [String] =
    Seq (
      soda_constant.package_reserved_word,
      soda_constant.import_reserved_word,
      soda_constant.class_end_reserved_word,
      soda_constant.class_reserved_word,
      soda_constant.comment_opening_symbol
    )

  lazy val coq_reserved_words =
    coq_1 ++ coq_2 ++ coq_3 ++ coq_4

  lazy val coq_1: Seq [String] =
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

  lazy val coq_2: Seq [String] =
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

  lazy val coq_3: Seq [String] =
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

  lazy val coq_4: Seq [String] =
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

  lazy val synonym_at_beginning: Seq [Tuple2 [String, String]] =
    Seq (
      Tuple2 ("*", "class"),
      Tuple2 ("+", "import")
    )

  lazy val synonym: Seq [Tuple2 [String, String]] =
    Seq (
      Tuple2 ("is", ":="),
      Tuple2 ("def", "")
    )

  lazy val main_translation: Seq [Tuple2 [String, String]] =
    Seq (
      Tuple2 (";", "."),
      Tuple2 (":", ":"),
      Tuple2 ("->", "->"),
      Tuple2 ("=", ":="),
      Tuple2 ("lambda", "fun"),
      Tuple2 ("if", "if"),
      Tuple2 ("then", "then"),
      Tuple2 ("else", "else"),
      Tuple2 ("let", "let"),
      Tuple2 ("in", "in"),
      Tuple2 ("match", "match"),
      Tuple2 ("case", "|"),
      Tuple2 ("end", "end"),
      Tuple2 ("|", "|"),
      Tuple2 ("false", "false"),
      Tuple2 ("true", "true"),
      Tuple2 ("not", "negb"),
      Tuple2 ("and", "andb"),
      Tuple2 ("or", "orb"),
      Tuple2 ("class", "Module Type"),
      Tuple2 ("has", "Parameter"),
      Tuple2 ("extends", "<:"),
      Tuple2 ("package", ""),
      Tuple2 ("import", "Require Import"),
      Tuple2 ("@override", ""),
      Tuple2 ("@tailrec", "")
    )

  lazy val type_translation: Seq [Tuple2 [String, String]] =
    Seq (
        Tuple2 ("Boolean", "bool"),
        Tuple2 ("Nat", "nat"),
        Tuple2 ("Option", "option"),
        Tuple2 ("List", "list"),
        Tuple2 ("String", "string"),
        Tuple2 ("BigInt", "Z")
    )

  lazy val prefix_coq_non_soda = "__soda__"

  lazy val coq_non_soda: Seq [Tuple2 [String, String]] =
    coq_reserved_words
      .filter (x => ! soda_constant.soda_reserved_words.contains (x )  )
      .map (x =>  (x, prefix_coq_non_soda + x ) )

  lazy val soda_brackets_and_comma =
    Seq ('(', ')', '[', ']', '{', '}', ',' )

  lazy val beautifier: Seq [Tuple2 [String, String]] =
    Seq (
      Tuple2 ("\\.\\s+", "."),
      Tuple2 ("=\\s+", "= "),
      Tuple2 ("\\s+=", " ="),
      Tuple2 ("\\(\\s+", "("),
      Tuple2 ("\\[\\s+", "["),
      Tuple2 ("\\s+\\]", "]"),
      Tuple2 ("\\s+,", ","),
      Tuple2 (",\\s+", ", "),
      Tuple2 ("\\s+:", " :"),
      Tuple2 (":\\s+", ": ")
    )

  def is_coq_word (word: String ): Boolean =
    coq_reserved_words.contains (word )

  def is_soda_word (word: String ): Boolean =
    soda_constant.soda_reserved_words.contains (word )

}

case class TranslationConstantToCoq_ ()
  extends
    TranslationConstantToCoq
{

}
