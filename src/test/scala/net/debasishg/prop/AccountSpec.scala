package net.debasishg.prop

import java.util.{ Date, Calendar }
import scalaz._
import Scalaz._

import scala.util.{ Try, Success, Failure }
import org.scalacheck._
import Prop.{ forAll, BooleanOperators }
import Gen._
import Arbitrary.arbitrary

import Banking._
import Money._
import Currency._

object AccountSpecification extends Properties("Account") {

  implicit val arbitraryDate: Arbitrary[Date] = Arbitrary {
    Gen.oneOf(today, today)
  }

  val genAccount = for {
    no <- Gen.oneOf("1", "2", "3")
    nm <- Gen.oneOf("john", "david", "mary")
    od <- arbitrary[Date]
    cd <- arbitrary[Option[Date]]
  } yield Account(no, nm, od, cd)

  val genCcy = Gen.oneOf(USD, SGD, AUD, INR)

  val genMoneyNZ = for {
    am <- Gen.oneOf(1 to 1000)
    cy <- genCcy
  } yield MoneyNZ(am, cy)

  val genZeroMoney = for {
    _ <- Gen.oneOf(1 to 10)
  } yield ZeroMoney

  implicit def arbitraryMoney: Arbitrary[Money] = Arbitrary {
    Gen.oneOf(genMoneyNZ, genZeroMoney)
  }

  val genDebit = for {
    am <- arbitrary[Money]
    dt <- arbitrary[Date]
  } yield Debit(am, dt)

  val genCredit = for {
    am <- arbitrary[Money]
    dt <- arbitrary[Date]
  } yield Credit(am, dt)

  implicit def arbitraryOperation: Arbitrary[Operation] = Arbitrary {
    Gen.oneOf(genDebit, genCredit)
  }

  implicit val arbitraryBalance = Arbitrary {
    for {
      ac <- genAccount
      am <- arbitrary[Money]
      dt <- arbitrary[Date]
    } yield Balance(ac, am, dt)
  }
    
  property("Credit succeeds for open accounts & fails for closed accounts") = forAll((b: Balance, m: Money) =>
    b.account.isClosed match {
      case true => b.adjust(Credit(m)).isFailure == true
      case _    => b.adjust(Credit(m)).map(_.current) == Success(b.current |+| m)
    }
  )

  val noBalanceForDebit = for {
    b <- arbitrary[Balance] suchThat (!_.account.isClosed)
    a <- arbitrary[Money] suchThat (MoneyOrder.greaterThan(_, b.current))
  } yield (b, a)

  val s = noBalanceForDebit.sample

  property("Debit fails for open accounts with insufficient balance") = forAll(s) { 
    case Some((b, a)) => b.adjust(Debit(a)).isFailure == true
    case None => 1 == 1
  }

  val validBalanceForDebit = for {
    b <- arbitrary[Balance] suchThat (!_.account.isClosed)
    a <- arbitrary[Money] suchThat (MoneyOrder.lessThanOrEqual(_, b.current))
  } yield (b, a)

  val t = validBalanceForDebit.sample

  property("Debit success for open accounts with sufficient balance") = forAll(t) { 
    case Some((b, a)) => b.adjust(Debit(a)).map(_.current) == Success(b.current |+| a.flip)
    case None => 1 == 1
  }
}
