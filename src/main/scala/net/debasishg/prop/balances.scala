package net.debasishg.prop

import scala.util.{ Try, Success, Failure }

object Balances {

  sealed trait Currency
  case object USD extends Currency
  case object AUD extends Currency
  case object SGD extends Currency
  case object INR extends Currency

  case class Money(amount: BigDecimal) {
    def +(m: Money): Money = Money(amount + m.amount)
  }

  object Money {
    def apply(m: Int): Money = Money(m: BigDecimal)
  }

  trait Monoid[T] {
    def zero: T
    def op(x: T, y: T): T
  }

  implicit class MonoidOps[T: Monoid](value: T) {
    def |+|(that: T) = Monoid[T].op(value, that)
  }

  object Monoid {
    def apply[T](implicit monoid: Monoid[T]) = monoid

    implicit val IntAdditionMonoid = new Monoid[Int] {
      val zero = 0
      def op(i: Int, j: Int): Int = i + j
    }

    implicit val StringMonoid = new Monoid[String] {
      val zero = ""
      def op(i: String, j: String) = i + j
    }

    implicit val BigDecimalAdditionMonoid = new Monoid[BigDecimal] {
      val zero: BigDecimal = 0
      def op(i: BigDecimal, j: BigDecimal) = i + j
    }

    implicit def ListMonoid[T] = new Monoid[List[T]] {
      val zero = List.empty
      def op(i: List[T], j: List[T]) = i ++ j
    }

    implicit def MapMonoid[K, V: Monoid] = new Monoid[Map[K, V]] {
      val zero = Map.empty[K, V]
      def op(i: Map[K, V], j: Map[K, V]): Map[K, V] = j.foldLeft(i) { (a, e) =>
        a.get(e._1).map { v => a + ((e._1, v |+| e._2)) }.getOrElse(a + ((e._1, e._2)))
      }
    }
  }    

  implicit val MoneyMonoid = new Monoid[Money] {
    def zero: Money = Money(0)
    def op(m1: Money, m2: Money): Money = m1 + m2
  }

  case class Account(no: String)

  def allCurrenciesBalance(m: Map[Account, Map[Currency, Money]]) = 
    m.values.foldLeft(Map.empty[Currency, Money])(_ |+| _)

  def allTotal[A, B, C: Monoid](m: Map[A, Map[B, C]]) = 
    m.values.foldLeft(Map.empty[B, C])(_ |+| _)

  val a1 = Account("1")
  val a2 = Account("2")
  val a3 = Account("3")

  val all: Map[Account, Map[Currency, Money]] = 
    Map(a1 -> Map(USD -> Money(1000), AUD -> Money(200), INR -> Money(10000)),
        a2 -> Map(USD -> Money(100), SGD -> Money(200)),
        a3 -> Map(INR -> Money(100), SGD -> Money(200)))

  /** employee wise monthly pay : and you want to find month wise disbursement **/
  val alg: Map[String, Map[Int, Int]] = 
    Map("a1" -> Map(0 -> 1000, 1 -> 200, 2 -> 10000),
        "a2" -> Map(0 -> 100, 3 -> 200),
        "a3" -> Map(2 -> 100, 3 -> 200))

}
