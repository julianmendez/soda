package soda.translator.extension.help

trait Help  extends soda.translator.extension.common.Extension {

  import soda.translator.io.SimpleFileReader_

  lazy val help: String =
    SimpleFileReader_ () .read_resource ("/soda/translator/documentation/help.txt")

  lazy val title_and_version: String =
    {
      lazy val packg = this.getClass.getPackage
      lazy val name = Option (packg.getImplementationTitle ) .getOrElse ("")
      lazy val version = Option (packg.getImplementationVersion ) .getOrElse ("")
      (name + " " + version ) .trim }

  def execute (arguments: Seq [String]  ): Boolean =
    {
      println (title_and_version + "\n\n" + help )
      true }

}

case class Help_ ()  extends Help
