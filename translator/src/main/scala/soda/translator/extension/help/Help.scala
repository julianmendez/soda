package soda.translator.extension.help

trait AbstractHelp
  extends
    soda.translator.extension.common.Extension
{

  def   execute : Seq [String] => Boolean

  import   soda.lib.SomeSD_
  import   soda.translator.io.SimpleFileReader_

  lazy val path : String = "/soda/translator/documentation/"

  def read (file_name : String) : String =
    SimpleFileReader_ ().read_resource (path + file_name)

  lazy val this_package = this.getClass.getPackage

  lazy val name : String = Option (this_package.getImplementationTitle).getOrElse ("")

  lazy val version : String = Option (this_package.getImplementationVersion).getOrElse ("")

  lazy val title_and_version : String = (name + " " + version).trim

  def output_content (content : String) : Boolean =
    SomeSD_ (content)
      .map (  x => println (x) )
      .map (  x => true)
      .getOrElse (false)

}

case class AbstractHelp_ (execute : Seq [String] => Boolean) extends AbstractHelp

trait Help
  extends
    AbstractHelp
{

  lazy val file_name = "help.txt"

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      output_content (title_and_version + "\n\n" + read (file_name) )

}

case class Help_ () extends Help

trait Manual
  extends
    AbstractHelp
{

  lazy val file_name = "Manual.soda"

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      output_content ("/* " + title_and_version + " */\n\n" + read (file_name) )

}

case class Manual_ () extends Manual

trait License
  extends
    AbstractHelp
{

  lazy val file_name = "LICENSE.txt"

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      output_content (read (file_name))

}

case class License_ () extends License
