package soda.coqport.language


/**
 * This class translates Soda source code into Scala source code.
 */
trait MicroTranslator {
  import soda.lib.SomeSD_

  lazy val tc = TranslationConstant_ ()

  lazy val new_line = "\n"

  def translate_program (program: String ): String =
    SomeSD_ (program )
      .value
}

case class MicroTranslator_ () extends MicroTranslator
