package net.debasishg.prop

import scala.util.{ Try, Success, Failure }
import java.util.Date

object smart {
  sealed trait DayOfWeek {
    val value: Int

    override def toString = value match {
      case 1 => "Monday"
      case 2 => "Tuesday"
      case 3 => "Wednesday"
      case 4 => "Thursday"
      case 5 => "Friday"
      case 6 => "Saturday"
      case 7 => "Sunday"
    }
  }

  object DayOfWeek {
    private def unsafeDayOfWeek(d: Int) = new DayOfWeek { val value = d }
    private val isValid: Int => Boolean = { i => i >= 1 && i <= 7 }
    def dayOfWeek(d: Int) = if (isValid(d)) Some(unsafeDayOfWeek(d)) else None
  }

  sealed trait Account {
    def no: String
    def name: String
    def openingDate: Date
    def closeDate: Option[Date]
  }

  final case class CheckingAccount private[prop](no: String, name: String, openingDate: Date, closeDate: Option[Date]) 
    extends Account

  final case class SavingsAccount private[prop](no: String, name: String, rateOfInterest: BigDecimal, openingDate: Date, 
    closeDate: Option[Date]) extends Account

  object Account {
    private val validDate: (Date, Date) => Boolean = { (d1, d2) => d1 before d2 }

/*
    def checkingAccount(no: String, name: String, openingDate: Date, 
      closeDate: Option[Date]): Either[String, Account] = { 

      if (closeDate.map(validDate(_, openingDate)).getOrElse(false)) 
        Left("Account close date [$closeDate] cannot be earlier than opening date [$openingDate]") 
      else Right(CheckingAccount(no, name, openingDate, closeDate))
    }

    def savingsAccount(no: String, name: String, rate: BigDecimal, openingDate: Date, 
      closeDate: Option[Date]): Either[String, Account] = { 

      if (closeDate.map(validDate(_, openingDate)).getOrElse(false))
        Left("Account close date [$closeDate] cannot be earlier than opening date [$openingDate]") 
      else if (rate <= BigDecimal(0)) 
        Left("Interest rate $rate must be > 0")
      else Right(SavingsAccount(no, name, rate, openingDate, closeDate))
    }
*/

    def checkingAccount(no: String, name: String, openingDate: Date, 
      closeDate: Option[Date] = None): Try[Account] = { 

      if (closeDate.map(validDate(_, openingDate)).getOrElse(false)) 
        Failure(new IllegalArgumentException(
          s"Account close date [$closeDate] cannot be earlier than opening date [$openingDate]")) 
      else Success(CheckingAccount(no, name, openingDate, closeDate))
    }

    def savingsAccount(no: String, name: String, rate: BigDecimal, openingDate: Date, 
      closeDate: Option[Date] = None): Try[Account] = { 

      if (closeDate.map(validDate(_, openingDate)).getOrElse(false))
        Failure(new IllegalArgumentException(
          s"Account close date [$closeDate] cannot be earlier than opening date [$openingDate]")) 
      else if (rate <= BigDecimal(0)) 
        Failure(new IllegalArgumentException(s"Interest rate $rate must be > 0"))
      else Success(SavingsAccount(no, name, rate, openingDate, closeDate))
    }
  }
}
