package soda.translator.extension.manual

trait Manual  extends soda.translator.extension.common.Extension {

  import soda.translator.io.SimpleFileReader_

  lazy val manual: String =
    SimpleFileReader_ () .read_resource ("/soda/translator/documentation/Manual.soda")

  lazy val title_and_version: String =
    {
      lazy val packg = this.getClass.getPackage
      lazy val name = Option (packg.getImplementationTitle ) .getOrElse ("")
      lazy val version = Option (packg.getImplementationVersion ) .getOrElse ("")
      (name + " " + version ) .trim }

  def execute (arguments: Seq [String]  ): Boolean =
    {
      println ("/* " + title_and_version + " */\n\n" + manual )
      true }

}

case class Manual_ ()  extends Manual
