package se.umu.cs.rai.scopus.translator

case class MyExample() {
  val swap = {
    pair: (Int, Int) => {
      val x = pair._1
      val y = pair._2
      (y, x)
    }
  }
}
