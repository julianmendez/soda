package soda.example


case class FlighMonitorSpec ()  extends org.scalatest.funsuite.AnyFunSuite {
  import soda.lib.OptionSD
  import soda.lib.SomeSD_

  def as_string (maybe_airport: OptionSD [Airport]  ): String =
    maybe_airport  match {
      case SomeSD_ (airport ) => airport.name
      case otherwise => ""
    }

  test ("Building valid airport identifiers") {
    lazy val builder = AirportBuilder ()
    lazy val obtained = Seq (builder.build ("UME"), builder.build ("ARN"), builder.build ("ESSA"), builder.build ("ESNU"), builder.build ("ume"), builder.build ("arn"), builder.build ("FRA")    ) .map (x => as_string (x ) )
    lazy val expected = Seq ("UME", "ARN", "", "", "", "", "FRA")

    assert (obtained == expected )
  }
}
