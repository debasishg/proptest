package net.debasishg.prop

import java.util.{ Date, Calendar }

import scala.util.{ Try, Success, Failure }

import scalaz._
import Scalaz._
import Ordering._
import Money._
import Currency._

object Banking {

  case class InsufficientBalance(msg: String) extends Exception(msg)
  case class ClosedAccount(msg: String) extends Exception(msg)

  val today = Calendar.getInstance.getTime

  case class Account(no: String, holder: String, openingDate: Date, closeDate: Option[Date]) {
    def isClosed = closeDate.isDefined
  }

  sealed trait Operation {
    def amount: Money
    def on: Date
  }

  object Operation {
    def debit(amount: Money, on: Date): Operation = Debit(amount, on)
  }

  case class Debit(amount: Money, on: Date = today) extends Operation
  case class Credit(amount: Money, on: Date = today) extends Operation
  case object ZeroOp extends Operation {
    val amount = MoneyMonoid.zero
    val on = today
  }

  implicit def OperationEqual(implicit mo: Order[Money]) = new Equal[Operation] {
    def equal(o1: Operation, o2: Operation): Boolean = (o1, o2) match {
      case (Debit(a1, d1), Debit(a2, d2))   => a1 === a2 && d1 == d2 
      case (Credit(a1, d1), Credit(a2, d2)) => a1 === a2 && d1 == d2
      case _                                => false
    }
  }

  implicit def OperationMonoid(implicit dt: Date, mo: Order[Money]) = new Monoid[Operation] {
    def zero: Operation = ZeroOp
    def append(o1: Operation, o2: => Operation): Operation = (o1, o2) match {
      case (ZeroOp, ZeroOp)                            => ZeroOp
      case (ZeroOp, x)                                 => x
      case (x, ZeroOp)                                 => x

      case (Debit(m1, d1), Credit(m2, d2)) if m1 >= m2 => Debit(m1 |+| m2.flip, dt)

      case (Debit(m1, d1), Credit(m2, d2)) if m1 <= m2 => Credit(m2 |+| m1.flip, dt)

      case (Credit(m1, d1), Debit(m2, d2)) if m1 >= m2 => Credit(m1 |+| m2.flip, dt)

      case (Credit(m1, d1), Debit(m2, d2)) if m1 <= m2 => Debit(m2 |+| m1.flip, dt)

      case (Credit(m1, d1), Credit(m2, d2))            => Credit(m1 |+| m2, dt)

      case (Debit(m1, d1), Debit(m2, d2))              => Debit(m1 |+| m2, dt)
    }
  }

  case class Balance(account: Account, current: Money, asOf: Date) {
    def adjust(op: Operation)(implicit mo: Order[Money]) = {
      if (account isClosed) Failure(ClosedAccount(s"Account ${account.no} is closed"))
      else op match {
          case Debit(by, on) => 
            if (current < by) Failure(InsufficientBalance(s"Insufficient balance [$current] to debit [$by]"))
            else Success(Balance(account, current |+| by.flip, on))

          case Credit(by, on) => Success(Balance(account, current |+| by, on))
          case ZeroOp => Success(this)
      }
    }
  }

  object BankingOps {
    def eodCashFlow[T <: Operation](account: Account, on: Date, ops: List[T])(implicit om: Monoid[T]): Map[Currency, T] = {
      ops.groupBy(_.amount.ccy).mapValues{ _.foldMap(identity) }
    }
  }
}
