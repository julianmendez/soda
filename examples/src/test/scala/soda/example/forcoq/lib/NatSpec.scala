package soda.example.forcoq.lib


case class NatSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("IntNat from non negative") {
    lazy val instance = IntNat_ ()
    lazy val expected = S (S (S (S (S (S (S (S (O ()  )  )  )  )  )  )  )  )
    lazy val obtained =
      instance.from_non_negative (8 )

    assert (obtained == expected )
  }

  test ("IntNat to Int") {
    lazy val instance = IntNat_ ()
    lazy val expected = 5
    lazy val obtained =
      instance.to_Int (S (S (S (S (S (O ()  )  )  )  )  )      )

    assert (obtained == expected )
  }

    test ("Nat add") {
      lazy val a = S (S (S (O ()  )  )  )
      lazy val b = S (S (S (S (S (O ()  )  )  )  )  )
      lazy val expected = S (S (S (S (S (S (S (S (O ()  )  )  )  )  )  )  )  )
      lazy val obtained = a.add (b )

      assert (obtained == expected )
    }

    test ("Nat mul") {
      lazy val a = S (S (S (O ()  )  )  )
      lazy val b = S (S (S (S (O ()  )  )  )  )
      lazy val expected = S (S (S (S (S (S (S (S (S (S (S (S (O ()  )  )  )  )  )  )  )  )  )  )  )  )
      lazy val obtained = a.mul (b )

      assert (obtained == expected )
    }
}
