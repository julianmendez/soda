/*
 * This file is automatically generated. Do not edit.
 */

package soda.lib

/**
 * This is a constant to be used in enumerations.
 */

trait EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class EnumConstant_ (ordinal : Int, name : String) extends EnumConstant

trait Enum [A <: EnumConstant]
{

  def   values : Seq [A]

}

case class Enum_ [A <: EnumConstant] (values : Seq [A]) extends Enum [A]
