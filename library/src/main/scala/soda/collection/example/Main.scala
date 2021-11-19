package soda.collection.example


object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

trait MainClass {

  def main (args: Array [String]  ): Unit =
    {
      lazy val e = ListExample_ ()
      lazy val all_examples = Seq (e.a_example, e.b_example, e.take_example, e.takeRight_example, e.takeWhile_example, e.drop_example, e.dropRight_example, e.dropWhile_example, e.splitAt_example, e.indices_example, e.zipWithIndex_example, e.zip_example, e.reverse_example, e.prepended_example, e.appended_example, e.concat_example, e.span_example, e.map_example, e.filter_example, e.fold_example, e.foldLeft_example, e.foldRight_example      )
        .map (pair => pair.name + " = " + pair.result.toString )
        .mkString ("\n")
      println (all_examples ) }

}

case class Main () extends MainClass
