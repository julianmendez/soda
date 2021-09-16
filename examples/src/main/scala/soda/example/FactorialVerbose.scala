package soda.example


trait AbstractFactorialVerbose {

  def get_factorial (n: Int ): Int
}

trait FactorialVerbose  extends AbstractFactorialVerbose {

  def get_factorial (n: Int ) =
    {
      lazy val result = rec (n, 1 )

      import scala.annotation.tailrec
        @tailrec
      def rec (n: Int, product: Int ): Int =
        if (n == 0
        ) product
        else rec (n - 1, n * product )
      result }
}

case class FactorialVerbose_ () extends FactorialVerbose
