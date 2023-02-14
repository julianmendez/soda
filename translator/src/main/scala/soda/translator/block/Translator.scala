package soda.translator.block

/*
 * This package contains abstract and concrete classes to define a block and a block translator.
 */





/**
 * This models an abstract translator.
 */

trait Translator
{

  def   translate : String => String
  def   keys : Seq [String]

}

case class Translator_ (translate : String => String, keys : Seq [String]) extends Translator
