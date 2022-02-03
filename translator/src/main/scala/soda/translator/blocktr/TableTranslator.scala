package soda.translator.blocktr

trait Table
{

  def   table: Seq [Tuple2 [String, String]]

}

trait TableTranslator
  extends
    Table
    with soda.translator.block.Translator
{

  def   table: Seq [Tuple2 [String, String]]

  lazy val keys = table.map (pair => pair._1 )

  lazy val translate: String => String =
     word =>
      table.toMap.get (word ) .getOrElse (word )

}

case class TableTranslator_ (table: Seq [Tuple2 [String, String]] )
  extends
    TableTranslator
{

}
