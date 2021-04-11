package soda.lib

/**
 * This class contains auxiliary functions for combinations.
 */
case class Comb () {

  def cartesianProduct [T]  (sequences: Seq [Seq [T]]  ): Seq [Seq [T]] = {
    lazy val result =
      if (sequences.isEmpty
      ) sequences
      else {
        lazy val rev_sequences = sequences.reverse
        Rec () .foldLeft (rev_sequences.tail, initial_value (rev_sequences.head ), next_value )
      }

    def initial_value (seq: Seq [T]  ): Seq [Seq [T]] = seq.map (elem => Seq (elem )  )

    def next_value (accum: Seq [Seq [T]], seq_a: Seq [T]  ): Seq [Seq [T]] =
      seq_a.flatMap (elem_a =>
        accum.map (seq_b => seq_b.+: (elem_a ) ) )

    result
  }

}
