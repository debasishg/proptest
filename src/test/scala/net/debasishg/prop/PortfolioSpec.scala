package net.debasishg.prop

import java.util.{ Date, Calendar }

import scala.util.{ Try, Success, Failure }
import org.scalacheck._
import Prop.{ forAll, BooleanOperators }
import Gen._
import Arbitrary.arbitrary

import smart._
import smart.Account._
import portfolios.{ Money => PMoney, _ }
import portfolios.Banking._

object PortfolioSpecification extends Properties("portfolio") {

  implicit val arbitraryBigDecimal: Arbitrary[BigDecimal] = Arbitrary {
    Gen.oneOf(BigDecimal(10), BigDecimal(20))
  }

  implicit val arbitraryDate: Arbitrary[Date] = Arbitrary {
    Gen.oneOf(today, today)
  }

  def arbDateGreaterThan(d: Date): Arbitrary[Date] = Arbitrary {
    for {
      x <- arbitrary[Date] suchThat (_ after d)
    } yield x
  }

  implicit val arbAccount: Arbitrary[Account] = Arbitrary {
    for {
      no <- Gen.oneOf("1", "2", "3")
      nm <- Gen.oneOf("john", "david", "mary")
      od <- arbitrary[Date]
    } yield checkingAccount(no, nm, od).get
  }

  implicit val arbCcy: Arbitrary[Currency] = Arbitrary { 
    Gen.oneOf(USD, SGD, AUD, INR)
  }

  implicit val arbMoney = Arbitrary {
    for {
      a <- Gen.oneOf(1 to 10)
      c <- arbitrary[Currency]
    } yield PMoney(a, c)
  }

  implicit val arbPosition: Arbitrary[Position] = Arbitrary {
    for {
      a <- arbitrary[Account]
      m <- arbitrary[PMoney]
      d <- arbitrary[Date]
    } yield Position(a, m, d)
  }

  property("Equal debit & credit retains the same position") = 
    forAll((a: Account, c: Currency, d: Date, i: BigDecimal) => { 
      val Success((before, after)) = for {
        p <- position(a, c, d)
        r <- credit(p, PMoney(i, c), d) 
        q <- debit(r, PMoney(i, c), d) 
      } yield (q, p)

      before == after
    })

  property("Can close account with close date > opening date") = forAll((a: Account) =>
    close(a, new Date(a.openingDate.getTime + 10000)).isSuccess == true
  )

  property("Cannot close account with close date < opening date") = forAll((a: Account) =>
    close(a, new Date(a.openingDate.getTime - 10000)).isSuccess == false
  )
}
