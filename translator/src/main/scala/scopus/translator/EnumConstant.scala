package scopus.translator

trait EnumConstant {
  lazy val ordinal: Int
  lazy val name: String

  override
  lazy val toString: String = "" + ordinal + "-" + name

}
