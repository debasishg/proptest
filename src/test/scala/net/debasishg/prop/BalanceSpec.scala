package net.debasishg.prop

import scalaz._
import Scalaz._
import org.scalacheck.Arbitrary
import org.specs2.scalaz._
import scalaz.scalacheck.ScalazProperties._


import Banking._
import AccountSpecification._

class BalanceSpec extends Spec {

  implicit val dt = today

  checkAll(order.laws[Money])
  checkAll(monoid.laws[Money])
  checkAll(equal.laws[Operation])
  checkAll(monoid.laws[Operation])
}
