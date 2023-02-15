package soda.translator.extension.common

/*
 * This package contains common classes used by the extensions.
 */



trait Package

trait Extension
{

  def   execute : Seq [String] => Boolean

}

case class Extension_ (execute : Seq [String] => Boolean) extends Extension

