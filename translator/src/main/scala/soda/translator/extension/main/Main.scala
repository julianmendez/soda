package soda.translator.extension.main

object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}


/**
 * This is the main entry point.
 */
case class Main ()  extends MainClass

trait MainClass  extends soda.translator.extension.common.Extension {

  import soda.translator.extension.common.Extension

  lazy val help = soda.translator.extension.help.Help_ ()

  lazy val extensions: Map [String, Extension] =
    Seq ((".", soda.translator.extension.toscala.TranslatorToScala_ () ), ("--scala", soda.translator.extension.toscala.TranslatorToScala_ () ), ("--coq", soda.translator.extension.tocoq.TranslatorToCoq_ () ), ("--help", help ), ("--manual", soda.translator.extension.manual.Manual_ () ), ("--main", this )    ) .toMap

  def main (arguments: Array [String]  ): Unit =
    if (arguments.length == 0
    ) help.execute (arguments.toSeq )
    else execute (arguments.toSeq )

  def execute (arguments: Seq [String]  ): Boolean =
    {
      lazy val extension_name = arguments.head
      lazy val new_arguments = arguments.tail
      lazy val extension_instance = extensions.getOrElse (extension_name, help )
      extension_instance.execute (new_arguments ) }

}
