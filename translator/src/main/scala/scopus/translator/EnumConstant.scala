package scopus.translator

trait EnumConstant {
  def ordinal: Int
  def name: String

  override
  lazy val toString: String = "" + ordinal + "-" + name

}
