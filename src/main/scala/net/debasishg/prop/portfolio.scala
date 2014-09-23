package net.debasishg.prop

import scala.util.{ Try, Success, Failure }
import java.util.{ Date, Calendar }

import scalaz._
import Scalaz._

object portfolios {

  val today = Calendar.getInstance.getTime

  trait Banking[Account, Position, Currency, Money] {
    sealed trait AccountType
    case object Checking extends AccountType
    case object Savings extends AccountType

    def open(no: String, name: String, openingDate: Date, rate: Option[BigDecimal], tp: AccountType): Try[Account]
    def close(account: Account, closeDate: Date): Try[Account]
    def debit(current: Position, money: Money, on: Date): Try[Position]
    def credit(current: Position, money: Money, on: Date): Try[Position]
    def position(account: Account, ccy: Currency, asOf: Date): Try[Position]

    object Laws {
      //.. @todo
    }
  }

  case class Money(amount: BigDecimal, val ccy: Currency) {
    def +(m: Money): Try[Money] = Try { 
      if (ccy != m.ccy) throw new IllegalArgumentException(s"Currencies need to match")
      else Money(amount + m.amount, ccy)
    }
    def -(m: Money): Try[Money] = Try { 
      if (ccy != m.ccy) throw new IllegalArgumentException(s"Currencies need to match")
      else Money(amount - m.amount, ccy)
    }
  }

  import smart._
  import Account._

  case class Position(account: Account, balance: Money, asOf: Date)

  object Banking extends Banking[Account, Position, Currency, Money] {
    def open(no: String, name: String, openingDate: Date, rate: Option[BigDecimal], 
      tp: AccountType): Try[Account] = tp match {

      case Checking => checkingAccount(no, name, openingDate)
      case Savings  => 
        rate.map { r => savingsAccount(no, name, r, openingDate) }
            .getOrElse(throw new IllegalArgumentException(s"Rate must be > 0"))
    }

    def close(account: Account, date: Date): Try[Account] = Try { 
      if (date before account.openingDate) 
        throw new IllegalArgumentException(
          s"Account close date [$date] cannot be earlier than opening date [${account.openingDate}]") 
      else account match {
        case c@CheckingAccount(_, _, _, closeDate) => c.copy(closeDate = Some(date))
        case s@SavingsAccount(_, _, _, _, closeDate) => s.copy(closeDate = Some(date))
      }
    }

    def debit(current: Position, money: Money, on: Date): Try[Position] = 
      if (current.balance.amount < money.amount) 
        Failure(new IllegalArgumentException(s"Insufficient balance ${current.balance} for debit $money"))
      else (current.balance - money).map { m =>
        current.copy(balance = m, asOf = on)
      }

    def credit(current: Position, money: Money, on: Date): Try[Position] = Try {
      current.copy(balance = Money(current.balance.amount + money.amount, money.ccy), asOf = on)
    }

    def position(account: Account, ccy: Currency, asOf: Date): Try[Position] = Try {
      Position(account, Money(BigDecimal(10000), ccy), asOf)
    }

    def op = for {
      a <- open("1", "dg", today, None, Checking)
      p <- position(a, USD, today)
      d <- debit(p, Money(100, USD), today) 
      c <- debit(d, Money(500, USD), today) 
    } yield c
  }
}
