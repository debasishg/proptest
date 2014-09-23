package net.debasishg.prop

import scalaz._
import Scalaz._
import Ordering._

import scala.util.{ Try, Success, Failure}

sealed trait Currency
case object USD extends Currency
case object AUD extends Currency
case object SGD extends Currency
case object INR extends Currency

object Currency {
  final val baseCcy = USD

  final val conv: Map[Currency, BigDecimal] = Map(
    USD -> BigDecimal(1), 
    AUD -> BigDecimal(1.2), 
    SGD -> BigDecimal(1.3), 
    INR -> BigDecimal(60.0)
  )
}

import Currency._
sealed trait Money {
  def amount: BigDecimal
  def ccy: Currency
  def toBaseCcy: Money
  def flip: Money
}

final case class MoneyNZ private[prop] (amount: BigDecimal, ccy: Currency) extends Money {
  def toBaseCcy = copy(amount = this.amount * conv(ccy), ccy = baseCcy)
  def flip = copy(amount = -this.amount)
}

case object ZeroMoney extends Money {
  final val amount = BigDecimal(0)
  final val ccy = baseCcy
  final val toBaseCcy = ZeroMoney
  final val flip = ZeroMoney
}

object Money {
  def apply(amount: BigDecimal, ccy: Currency): Try[Money] = Try {
    if (amount < 0) throw new IllegalArgumentException("Amount [$amount] must be positive")
    else if (amount == 0 && ccy == baseCcy) ZeroMoney
    else MoneyNZ(amount, ccy)
  }

  implicit object MoneyOrder extends Order[Money] {
    def order(m1: Money, m2: Money): Ordering = (m1, m2) match { 
      case (ZeroMoney, ZeroMoney) => Ordering.EQ
      case (ZeroMoney, _) => Ordering.LT
      case (_, ZeroMoney) => Ordering.GT
      case (MoneyNZ(a1, c1), MoneyNZ(a2, c2)) if c1 == c2 => { 
        if (a1 < a2) LT 
        else if (a1 == a2) EQ
        else GT
      }
      case (x@MoneyNZ(a1, c1), y@MoneyNZ(a2, c2)) => {
        val b1 = x.toBaseCcy
        val b2 = y.toBaseCcy
        if (b1.amount < b2.amount) LT
        else if (b1.amount == b2.amount) EQ
        else GT
      }
    }
  }
  
  implicit object MoneyMonoid extends Monoid[Money] {
    def zero: Money = ZeroMoney
    def append(m1: Money, m2: => Money): Money = (m1, m2) match {
      case (ZeroMoney, ZeroMoney) => ZeroMoney
      case (ZeroMoney, m) => m.toBaseCcy
      case (m, ZeroMoney) => m.toBaseCcy
      case (MoneyNZ(a1, c1), MoneyNZ(a2, c2)) if c1 == c2 => MoneyNZ(a1 + a2, c1).toBaseCcy
      case (x@MoneyNZ(a1, c1), y@MoneyNZ(a2, c2)) => MoneyNZ(x.toBaseCcy.amount + y.toBaseCcy.amount, baseCcy)
    }
  }
}

