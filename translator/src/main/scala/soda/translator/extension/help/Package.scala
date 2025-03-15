package soda.translator.extension.help

/*
 * This package is for the help extension.
 */

trait AbstractHelp
  extends
    soda.translator.extension.common.Extension
{

  def   execute : Seq [String] => Boolean

  import   soda.lib.SomeSD
  import   soda.translator.io.SimpleFileReader

  lazy val path : String = "/soda/translator/documentation/"

  def read (file_name : String) : String =
    SimpleFileReader .mk .read_resource (path + file_name)

  lazy val this_package = this .getClass .getPackage

  lazy val name : String = Option (this_package .getImplementationTitle) .getOrElse ("")

  lazy val version : String = Option (this_package .getImplementationVersion) .getOrElse ("")

  lazy val title_and_version : String = (name + " " + version) .trim

  def output_content (content : String) : Boolean =
    SomeSD .mk [String] (content)
      .map ( x => println (x) )
      .map ( x => true)
      .getOrElse (false)

}

case class AbstractHelp_ (execute : Seq [String] => Boolean) extends AbstractHelp

object AbstractHelp {
  def mk (execute : Seq [String] => Boolean) : AbstractHelp =
    AbstractHelp_ (execute)
}

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

object Help {
  def mk : Help =
    Help_ ()
}

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

object Manual {
  def mk : Manual =
    Manual_ ()
}

trait License
  extends
    AbstractHelp
{

  lazy val license_file_name = "soda-license.txt"

  lazy val notice_file_name = "soda-notice.txt"

  private lazy val _separator = "\n\n---\n\n"

  lazy val execute : Seq [String] => Boolean =
     arguments =>
      output_content (
        read (notice_file_name) +
        _separator +
        read (license_file_name)
      )

}

case class License_ () extends License

object License {
  def mk : License =
    License_ ()
}

