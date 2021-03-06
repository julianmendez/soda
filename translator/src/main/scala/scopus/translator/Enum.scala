package scopus.translator

trait Enum {
  val index: Int
  val name: String

  override
  val toString: String = "" + index + "-" + name

}
