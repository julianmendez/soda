package soda.translator.extension.common

trait Extension
{

  def   execute: Seq [String] => Boolean

}

case class Extension_ (execute: Seq [String] => Boolean) extends Extension
