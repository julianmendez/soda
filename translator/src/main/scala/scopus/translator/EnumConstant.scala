package scopus.translator

trait EnumConstant {
  val ordinal: Int
  val name: String

  override
  val toString: String = "" + ordinal + "-" + name

}
