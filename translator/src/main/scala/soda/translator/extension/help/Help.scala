package soda.translator.extension.help

trait Help  extends soda.translator.extension.common.Extension {

  import soda.translator.io.SimpleFileReader_

  def execute (arguments: Seq [String]  ): Boolean

  lazy val path: String = "/soda/translator/documentation/"

  def read (file_name: String ): String =
    SimpleFileReader_ () .read_resource (path + file_name )

  lazy val this_package = this.getClass.getPackage

  lazy val name: String = Option (this_package.getImplementationTitle ) .getOrElse ("")

  lazy val version: String = Option (this_package.getImplementationVersion ) .getOrElse ("")

  lazy val title_and_version: String = (name + " " + version ) .trim

  def output_content (content: String ): Boolean =
    println (content ) .asInstanceOf [Boolean]

}

case class Help_ ()  extends Help {

  lazy val file_name = "help.txt"

  def execute (arguments: Seq [String]  ): Boolean =
    output_content (title_and_version + "\n\n" + read (file_name ) )

}

case class Manual_ ()  extends Help {

  lazy val file_name = "Manual.soda"

  def execute (arguments: Seq [String]  ): Boolean =
    output_content ("/* " + title_and_version + " */\n\n" + read (file_name ) )

}

case class License_ ()  extends Help {

  lazy val file_name = "LICENSE.txt"

  def execute (arguments: Seq [String]  ): Boolean =
    output_content (read (file_name )  )

}
