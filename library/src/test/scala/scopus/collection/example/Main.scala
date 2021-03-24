package scopus.collection.example

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

case class Main (  ) {

  def main ( args: Array [ String ]  ) : Unit = {
    lazy val e = ListExample (  )
    lazy val allExamples = Seq (
      e.aExample ,      e.bExample ,      e.takeExample ,      e.takeRightExample ,      e.takeWhileExample ,      e.dropExample ,      e.dropRightExample ,      e.dropWhileExample ,      e.splitAtExample ,      e.zipWithIndexExample ,      e.indicesExample ,      e.zipExample ,      e.reverseExample ,      e.prependExample ,      e.appendExample ,      e.concatExample ,      e.spanExample ,      e.mapExample ,      e.filterExample ,      e.foldExample ,      e.foldLeftExample ,      e.foldRightExample
    )
      .map ( pair => pair._1 + " = " + pair._2.toString )
      .mkString ("\n")
    println ( allExamples )
  }

}
