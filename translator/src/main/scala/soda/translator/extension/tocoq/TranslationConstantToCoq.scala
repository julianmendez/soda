package soda.translator.extension.tocoq

/**
 * This class contains constants that are specific for the Soda translator, like reserved words for Soda and Scala.
 */
trait TranslationConstantToCoq  extends soda.translator.extension.fromsoda.SodaConstant {

  lazy val soda_definition: String = "="

  lazy val soda_colon: String = ":"

  lazy val soda_opening_parenthesis: String = "("

  lazy val soda_closing_parenthesis: String = ")"

  lazy val soda_opening_brace: String = "{"

  lazy val soda_closing_brace: String = "}"

  lazy val soda_opening_comment: String = "/*"

  lazy val soda_closing_comment: String = "*/"

  lazy val scala_3_class_definition: String = ":"

  lazy val soda_in_reserved_word: String = "in"

  lazy val soda_in_pattern: String = soda_in_reserved_word + " "

  lazy val soda_let_reserved_word: String = "let"

  lazy val soda_let_pattern: String = soda_let_reserved_word + " "

  lazy val scala_in_translation: String = " }"

  lazy val soda_in_let_pattern: String = soda_in_reserved_word + " " + soda_let_reserved_word + " "

  lazy val scala_in_let_translation: String = " "

  lazy val soda_match_reserved_word: String = "match"

  lazy val soda_match_pattern: String = soda_match_reserved_word + " "

  lazy val scala_match_translation: String = " match "

  lazy val soda_class_reserved_word: String = "class"

  lazy val soda_package_reserved_word: String = "package"

  lazy val soda_import_reserved_word: String = "import"

  lazy val soda_has_reserved_word: String = "has"

  lazy val coq_definition: String = "Definition"

  lazy val coq_value: String = "Definition"

  lazy val coq_recursive_definition: String = "Fixpoint"

  lazy val coq_definition_end: String = "."

  lazy val coq_recursive_definition_end: String = "."

  lazy val coq_with_reserved_word: String = "with"

  lazy val coq_recursive_function_prefixes: Seq [String] =
    Seq ("rec_", "_rec_", "tailrec_", "_tailrec_", "@tailrec"    )

  lazy val non_definition_block_prefixes: Seq [String] =
    Seq (soda_package_reserved_word, soda_import_reserved_word, soda_closing_brace, soda_class_reserved_word, soda_opening_comment    )

  lazy val coq_reserved_words =
    coq_1 ++ coq_2 ++ coq_3 ++ coq_4

  lazy val coq_1: Seq [String] =
    Seq ("as", "else", "end", "forall", "fun", "if", "in", "let", "match", "then", "with"    )

  lazy val coq_2: Seq [String] =
    Seq ("Admitted", "Arguments", "Check", "Constructors", "End", "Eval", "Export", "Hint", "Implicit", "Import", "Module", "Notation", "Print", "Proof", "Qed", "Require", "Resolve", "Section", "Set", "Unset"    )

  lazy val coq_3: Seq [String] =
    Seq ("admit", "apply", "assert", "auto", "case", "compute", "destruct", "discriminate", "elim", "exact", "induction", "intros", "pose", "refine", "rewrite", "simpl", "specialize", "unfold"    )

  lazy val coq_4: Seq [String] =
    Seq ("CoFixpoint", "CoInductive", "Definition", "Example", "Fixpoint", "Global", "Hypothesis", "Inductive", "Instance", "Lemma", "Ltac", "Theorem", "Variable"    )

  lazy val synonym_at_beginning: Seq [(String, String )] = Seq (("*", "class"), ("+", "import")  )

  lazy val synonym: Seq [(String, String )] = Seq (("is", ":=")  )

  lazy val main_translation: Seq [(String, String )] = Seq ((";", "."), (":", ":"), ("->", "->"), ("=", ":="), ("lambda", "fun"), ("if", "if"), ("then", "then"), ("else", "else"), ("let", "let"), ("in", "in"), ("match", "match"), ("case", "|"), ("end", "end"), ("|", "|"), ("false", "false"), ("true", "true"), ("not", "negb"), ("and", "andb"), ("or", "orb"), ("class", "Module"), ("has", "Definition"), ("package", ""), ("import", "Require Import"), ("@override", ""), ("@tailrec", ""), ("@main", "")  )

  lazy val type_translation: Seq [(String, String )] = Seq (("Boolean", "bool"), ("Nat", "nat"), ("Option", "option"), ("List", "list"), ("String", "string"), ("BigInt", "Z")  )

  lazy val prefix_coq_non_soda = "__soda__"

  lazy val coq_non_soda: Seq [(String, String )] =
    coq_reserved_words
      .filter (x => ! soda_reserved_words.contains (x )  )
      .map (x =>  (x, prefix_coq_non_soda + x ) )

  lazy val soda_brackets_and_comma = Seq ('(', ')', '[', ']', '{', '}', ',' )

  lazy val beautifier: Seq [(String, String )] = Seq (("\\.\\s+", "."), ("=\\s+", "= "), ("\\s+=", " ="), ("\\(\\s+", "("), ("\\[\\s+", "["), ("\\s+\\]", "]"), ("\\s+,", ","), (",\\s+", ", "), ("\\s+:", " :"), (":\\s+", ": ")  )

  def is_coq_word (word: String ): Boolean =
    coq_reserved_words.contains (word )

  def is_soda_word (word: String ): Boolean =
    soda_reserved_words.contains (word )

}

case class TranslationConstantToCoq_ ()  extends TranslationConstantToCoq
