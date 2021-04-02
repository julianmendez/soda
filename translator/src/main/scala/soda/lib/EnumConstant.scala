package scopus.lib

/**
 * This is a constant to be used in enumerations.
 */
trait EnumConstant {
  def ordinal: Int
  def name: String

  override
  lazy val toString: String = "" + ordinal + "-" + name

}
