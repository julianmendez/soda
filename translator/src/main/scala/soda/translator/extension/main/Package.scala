package soda.translator.extension.main

/*
 * This package is for the main extension.
 * This is the entry point when the application is executed from a terminal.
 */

/**
 * This is the main entry point.
 */

trait Main
  extends
    soda.translator.extension.common.Extension
{

  import   soda.translator.extension.common.Extension



  lazy val help = soda.translator.extension.help.Help .mk

  lazy val extensions : Map [String, Extension] =
    Seq (
      ("." , soda.translator.extension.toscala.TranslatorToScala .mk ),
      ("scala" , soda.translator.extension.toscala.TranslatorToScala .mk ),
      ("lean" , soda.translator.extension.tolean.TranslatorToLean .mk ),
      ("coq" , soda.translator.extension.tocoq.TranslatorToCoq .mk ),
      ("doc" , soda.translator.extension.todoc.TranslatorToDoc .mk ),
      ("manual" , soda.translator.extension.help.Manual .mk ),
      ("license" , soda.translator.extension.help.License .mk ),
      ("help" , help ),
      ("-h" , help ),
      ("--help" , help ),
      ("main" , this )
    ) .toMap

  def execute_for (arguments : Seq [String] ) : Boolean =
    if ( arguments .isEmpty
    ) help .execute (arguments)
    else
      extensions
        .getOrElse (arguments .head , help)
        .execute (arguments .tail)

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      execute_for (arguments)

  def main (arguments : Array [String] ) : Unit =
    execute (arguments .toSeq)

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main {
  def mk : Main =
    Main_ ()
}

