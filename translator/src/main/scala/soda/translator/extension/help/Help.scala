package soda.translator.extension.help

trait Help  extends soda.translator.extension.common.Extension {

  import soda.translator.io.SimpleFileReader_

  def execute (arguments: Seq [String]  ): Boolean

  lazy val path: String = "/soda/translator/documentation/"

  def read (file_name: String ): String =
    SimpleFileReader_ () .read_resource (path + file_name )

  lazy val title_and_version: String =
    {
      lazy val packg = this.getClass.getPackage
      lazy val name = Option (packg.getImplementationTitle ) .getOrElse ("")
      lazy val version = Option (packg.getImplementationVersion ) .getOrElse ("")
      (name + " " + version ) .trim }

}

case class Help_ ()  extends Help {

  lazy val file_name = "help.txt"

  def execute (arguments: Seq [String]  ): Boolean =
    {
      println (title_and_version + "\n\n" + read (file_name ) )
      true }

}

case class Manual_ ()  extends Help {

  lazy val file_name = "Manual.soda"

  def execute (arguments: Seq [String]  ): Boolean =
    {
      println ("/* " + title_and_version + " */\n\n" + read (file_name ) )
      true }

}

case class License_ ()  extends Help {

  lazy val file_name = "LICENSE.txt"

  def execute (arguments: Seq [String]  ): Boolean =
    {
      println (read (file_name )  )
      true }

}
