package soda.translator.extension.main

object EntryPoint {
  def main (args: Array [String]): Unit = Main ().main (args)
}


/**
 * This is the main entry point.
 */

case class Main ()
  extends
    MainClass
{

}

trait MainClass
  extends
    soda.translator.extension.common.Extension
{

  import   soda.translator.extension.common.Extension

  lazy val help = soda.translator.extension.help.Help_ ()

  lazy val extensions: Map [String, Extension] =
    Seq (
      (".", soda.translator.extension.toscala.TranslatorToScala_ () ),
      ("scala", soda.translator.extension.toscala.TranslatorToScala_ () ),
      ("coq", soda.translator.extension.tocoq.TranslatorToCoq_ () ),
      ("manual", soda.translator.extension.help.Manual_ () ),
      ("license", soda.translator.extension.help.License_ () ),
      ("help", help ),
      ("-h", help ),
      ("--help", help ),
      ("main", this )
    ) .toMap

  def main (arguments: Array [String]  ): Unit =
    execute (arguments.toSeq )

  def execute (arguments: Seq [String]  ): Boolean =
    if (arguments.length == 0
    ) help.execute (arguments.toSeq )
    else
      extensions
        .getOrElse (arguments.head, help )
        .execute (arguments.tail )

}
