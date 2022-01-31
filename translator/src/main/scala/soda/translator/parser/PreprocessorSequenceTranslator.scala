package soda.translator.parser

trait PreprocessorSequenceTranslator
  extends
    soda.translator.block.BlockSequenceTranslator
{

  def   translator: soda.translator.block.BlockSequenceTranslator

  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.AnnotatedBlock_
  import   soda.translator.block.BlockAnnotationEnum_

  lazy val block_annotator = BlockAnnotator_ ()

  lazy val ba = soda.translator.block.BlockAnnotationEnum_ ()

  lazy val sc = SodaConstant_ ()

  lazy val recursion = soda.lib.Recursion_ ()

  lazy val empty_line = AnnotatedLine_ ("", true )

  lazy val translate: Seq [AnnotatedBlock] => Seq [AnnotatedBlock] =
     block_sequence =>
      translate_for (block_sequence )

  def translate_for (block_sequence: Seq [AnnotatedBlock]  ): Seq [AnnotatedBlock] =
    translator.translate (
      _get_second_pass (
        _get_first_pass (block_sequence )
      )
    )

  def _get_first_pass (block_sequence: Seq [AnnotatedBlock] ): Seq [AnnotatedBlock] =
    block_sequence.map (block => block_annotator.translate (block ) )

  def _get_second_pass (block_sequence: Seq [AnnotatedBlock] ): Seq [AnnotatedBlock] =
    recursion
      .fold (
        block_sequence.indices,
        _initial_value (block_sequence ),
        _next_value_function
      )
      .accumulated
      .reverse

  def _initial_value (block_sequence: Seq [AnnotatedBlock] ): AuxiliaryTuple =
    AuxiliaryTuple_ (
      block_sequence = block_sequence,
      accumulated = Seq [AnnotatedBlock] ()
    )

  def _next_value_function (current: AuxiliaryTuple, index: Int ): AuxiliaryTuple =
    _pass_next_step (current, index, _get_additional_information (current, index ) )

  def _get_additional_information (current: AuxiliaryTuple, index: Int ): Option [AnnotatedLine] =
    if ((current.block_sequence.apply (index ) .block_annotation == ba.class_end )
    ) _not_enabled_yet
    else None

  /*
  _get_as_comment (_get_class_name (current, index) )
  */

  lazy val _not_enabled_yet = None

  def _pass_next_step (current: AuxiliaryTuple, index: Int, maybe_annotated_line: Option [AnnotatedLine] ): AuxiliaryTuple =
    AuxiliaryTuple_ (
      block_sequence = current.block_sequence,
      accumulated = current.accumulated.+: (_append_line (current.block_sequence.apply (index ), maybe_annotated_line ) )
    )

  def _append_line (block: AnnotatedBlock, maybe_annotated_line: Option [AnnotatedLine] ): AnnotatedBlock =
    if (maybe_annotated_line.isEmpty
    ) block
    else AnnotatedBlock_ (block.annotated_lines.+: (maybe_annotated_line.get ), block.block_annotation )

  def _get_class_name (current: AuxiliaryTuple, index: Int ): Option [AnnotatedLine] =
    current
      .block_sequence
      .indices
      .takeWhile (k => (k < current.block_sequence.length ) && (k <= index ) )
      .filter (k => current.block_sequence.apply (k ) .block_annotation == ba.class_beginning )
      .map (k => current.block_sequence.apply (k ) )
      .lastOption
      .flatMap (annotated_block => annotated_block.annotated_lines.headOption )

  def _get_as_comment (maybe_annotated_line: Option [AnnotatedLine]  ): Option [AnnotatedLine] =
    if (maybe_annotated_line.isEmpty
    ) maybe_annotated_line
    else
      Some (
        AnnotatedLine_ (
          sc.comment_open + sc.space + maybe_annotated_line.get.line + sc.space + sc.comment_close,
          true
        )
      )

  /* FIXME this does not consider classes inside classes */

}

case class PreprocessorSequenceTranslator_ (translator: soda.translator.block.BlockSequenceTranslator )
  extends
    PreprocessorSequenceTranslator
{

}

trait AuxiliaryTuple
{

  def   block_sequence: Seq [soda.translator.block.AnnotatedBlock]
  def   accumulated: Seq [soda.translator.block.AnnotatedBlock]

}

case class AuxiliaryTuple_ (block_sequence: Seq [soda.translator.block.AnnotatedBlock], accumulated: Seq [soda.translator.block.AnnotatedBlock] )
  extends
    AuxiliaryTuple
{

}
