package soda.example


trait AbstractFactorialConcise {

  def get_factorial (n: Int ): Int
}

trait FactorialConcise  extends AbstractFactorialConcise {

  def get_factorial (n: Int ): Int =
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

case class FactorialConcise_ ()  extends FactorialConcise
