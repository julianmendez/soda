package soda.translator.extension.tocoq

trait CoqDefinitionBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.block.Block_

  lazy val source = "soda definitions"

  lazy val target = "coq definitions"

  lazy val space = " "

  lazy val tc = TranslationConstantToCoq_ ()

  def translate (block: Block ): Block =
    if (is_a_recursive_definition (block ) ) append (tc.coq_recursive_definition_end, prepend (tc.coq_recursive_definition + space, block ) )
    else if (is_a_definition (block ) ) append (tc.coq_definition_end, prepend (tc.coq_definition + space, block ) )
    else block

  def prepend (prefix: String, block: Block ): Block =
    Block_ (Seq [String] (prefix + block.lines.head ) ++ block.lines.tail    )

  def append (suffix: String, block: Block ): Block =
    Block_ (block.lines.:+ (suffix )    )

  def is_a_recursive_definition (block: Block ): Boolean =
    {
      lazy val first = block.lines.head.trim
      lazy val result = tc.coq_recursive_function_prefixes.exists (prefix => first.startsWith (prefix )  )
      result }

  def is_a_definition (block: Block ): Boolean =
    {
      lazy val contents = block.contents.trim
      lazy val result =
        ! is_a_recursive_definition (block ) &&
        ! tc.non_definition_block_prefixes.exists (prefix => contents.startsWith (prefix )  )
      result }

}

case class CoqDefinitionBlockTranslator_ ()  extends CoqDefinitionBlockTranslator
