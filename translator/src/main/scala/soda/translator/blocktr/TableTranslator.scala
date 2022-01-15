package soda.translator.blocktr

trait Table {

  def table: Seq [(String, String )]

}

trait TableTranslator  extends Table  with soda.translator.block.Translator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Token

  lazy val keys = table.map (pair => pair._1 )

  def translate (word: String ): String =
    table.toMap.get (word ) .getOrElse (word )

}

case class TableTranslator_ (table: Seq [(String, String )]  )  extends TableTranslator
