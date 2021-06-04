package soda.collection.example


object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

case class Main () {

  def main (args: Array [String]  ): Unit =
    {
      lazy val e = ListExample ()
      lazy val allExamples = Seq (e.aExample, e.bExample, e.takeExample, e.takeRightExample, e.takeWhileExample, e.dropExample, e.dropRightExample, e.dropWhileExample, e.splitAtExample, e.indicesExample, e.zipWithIndexExample, e.zipExample, e.reverseExample, e.prependedExample, e.appendedExample, e.concatExample, e.spanExample, e.mapExample, e.filterExample, e.foldExample, e.foldLeftExample, e.foldRightExample      )
        .map (pair => pair.name + " = " + pair.result.toString )
        .mkString ("\n")
      println (allExamples ) }
}
