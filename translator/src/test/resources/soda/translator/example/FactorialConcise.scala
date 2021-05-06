package soda.translator.example


trait AbstractFactorialConcise {
  def factorial (n: Int ): Int
}

case class FactorialConcise () extends AbstractFactorialConcise {

  def factorial (n: Int ) = {
    lazy val result = rec (n, 1 )

    import scala.annotation.tailrec
        @tailrec
    def rec (n: Int, product: Int ): Int =
      if (n == 0
      ) product
      else rec (n - 1, n * product )

    result
  }

}
