package soda.example.mathematics

/*
 * This package contains examples in Soda.
 * These examples use mathematical properties.
 */



trait FactorialSimple
{

  def apply (n : Int) : Int =
    if ( n < 2
    ) 1
    else n * apply (n - 1)

}

case class FactorialSimple_ () extends FactorialSimple
