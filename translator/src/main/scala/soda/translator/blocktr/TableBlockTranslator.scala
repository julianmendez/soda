package soda.translator.blocktr

trait Table {

  def table: Seq [(String, String )]

}

trait TableBlockTranslator  extends Table  with soda.translator.block.Translator  with soda.translator.block.BlockTranslator {

  import soda.translator.block.Block

  lazy val source = "soda"

  lazy val target = "soda"

  lazy val keys = table.map (pair => pair._1 )

  def translate (word: String ): String =
    table.toMap.get (word ) .getOrElse (word )

  /* FIXME */
  def translate (block: Block ): Block =
    block

}

case class TableBlockTranslator_ (table: Seq [(String, String )]  )  extends TableBlockTranslator
