package soda.example.forcoq.lib


case class NatSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("IntNat from non negative") {
    lazy val instance = IntNat_ ()
    lazy val expected = nat_S (nat_S (nat_S (nat_S (nat_S (nat_S (nat_S (nat_S (nat_O ()  )  )  )  )  )  )  )  )
    lazy val obtained =
      instance.from_non_negative (8 )

    assert (obtained == expected )
  }

  test ("IntNat to Int") {
    lazy val instance = IntNat_ ()
    lazy val expected = 5
    lazy val obtained =
      instance.to_Int (nat_S (nat_S (nat_S (nat_S (nat_S (nat_O ()  )  )  )  )  )      )

    assert (obtained == expected )
  }

    test ("Nat plus") {
      lazy val a = nat_S (nat_S (nat_S (nat_O ()  )  )  )
      lazy val b = nat_S (nat_S (nat_S (nat_S (nat_S (nat_O ()  )  )  )  )  )
      lazy val expected = nat_S (nat_S (nat_S (nat_S (nat_S (nat_S (nat_S (nat_S (nat_O ()  )  )  )  )  )  )  )  )
      lazy val obtained = a.plus (b )

      assert (obtained == expected )
    }
}
