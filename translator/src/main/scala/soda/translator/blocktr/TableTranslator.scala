package soda.translator.blocktr

trait Table
{

  def   table: Seq [(String, String )]

}

trait TableTranslator
  extends
    Table
    with soda.translator.block.Translator
{

  lazy val keys = table.map (pair => pair._1 )

  lazy val translate: String => String =
     word =>
      table.toMap.get (word ) .getOrElse (word )

}

case class TableTranslator_ (table: Seq [(String, String )]  )
  extends
    TableTranslator
{

}
