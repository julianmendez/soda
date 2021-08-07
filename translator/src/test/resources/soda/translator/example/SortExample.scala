package soda.translator.example


trait SortExample {

  def is_sorted (sequence: Seq [Int]  ): Boolean
}

trait SortExampleNaive {

  def is_sorted_with_at (sequence: Seq [Int]  ): Boolean =
    sequence
      .indices
      .filter (index => index > 0 )
      .forall (index => sequence (index - 1 ) <= sequence (index )  )

  def is_sorted_with_zip (sequence: Seq [Int]  ): Boolean =
    sequence
      .zip (sequence.tail )
      .forall (pair => (pair._1 <= pair._2 )  )
}

case class SortExampleNaive_ () extends SortExampleNaive
