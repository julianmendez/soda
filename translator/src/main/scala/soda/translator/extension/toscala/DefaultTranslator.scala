package soda.translator.extension.toscala

trait Table {

  def table: Seq [(String, String )]

}

trait DefaultTranslator  extends Table  with soda.translator.block.Translator {

  lazy val keys = table.map (pair => pair._1 )

  def translate (word: String ): String =
    table.toMap.get (word ) .getOrElse (word )

}

case class DefaultTranslator_ (table: Seq [(String, String )]  )  extends DefaultTranslator
