import java.time._
import java.time.temporal.ChronoUnit.{DAYS, MONTHS, WEEKS, YEARS}
import java.time.temporal.{ChronoUnit, Temporal}

import Utils._

object Utils {

  def doesRecurOnFrequency(unit: ChronoUnit)(from: Temporal, to: Temporal, frequency: Int): Boolean = unit.between(from, to) % frequency == 0

  def doesRecurMonthly: (Temporal, Temporal, Int) => Boolean = doesRecurOnFrequency(MONTHS)

  def doesRecurDaily: (Temporal, Temporal, Int) => Boolean = doesRecurOnFrequency(DAYS)

  def doesRecurYearly: (Temporal, Temporal, Int) => Boolean = doesRecurOnFrequency(YEARS)

  def isDayOfMonth(date: LocalDate, dayOfMonth: Int): Boolean = date.getDayOfMonth == dayOfMonth

  def isDayOfWeek(date: LocalDate, dayOfWeek: DayOfWeek): Boolean = date.getDayOfWeek == dayOfWeek

  def isMonthOfYear(date: LocalDate, monthOfYear: Int): Boolean = date.getMonthValue == monthOfYear

}

object TimeExpression {

  /**
    * This expression matches on the date of parameter value.
    *
    * @param localDate a local date
    * @return a TimeExpression
    */
  def apply(localDate: LocalDate): TimeExpression = new NonRecurringTimeExpression(localDate)

  def daily(every: Int, localDate: LocalDate): TimeExpression = new DailyRecurringTimeExpression(localDate, every)

  def monthlyEvery(every: Int, dayOfMonth: Int, from: YearMonth): TimeExpression = new MonthlyRecurringOnDayOfMonthTimeExpression(from, every, dayOfMonth)

  def monthlyEvery(every: Int, dayOfWeek: DayOfWeek, event: Event, from: YearMonth): TimeExpression = new MonthlyRecurringOnEventTimeExpression(from, every, MONTHS, dayOfWeek, event)

  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression = new YearlyRecurringTimeExpression(fromYear, amountOfYears, day)

}

trait TimeExpression {
  def isRecurringOn(otherDate: LocalDate): Boolean
}

class NonRecurringTimeExpression(localDate: LocalDate) extends TimeExpression {
  override def isRecurringOn(otherDate: LocalDate): Boolean = localDate == otherDate
}

class DailyRecurringTimeExpression(localDate: LocalDate, frequency: Int) extends TimeExpression {
  override def isRecurringOn(otherDate: LocalDate): Boolean = doesRecurDaily(localDate, otherDate, frequency)
}

class MonthlyRecurringOnDayOfMonthTimeExpression(localDate: Temporal, frequency: Int, dayOfMonth: Int) extends TimeExpression {
  override def isRecurringOn(otherDate: LocalDate): Boolean = doesRecurMonthly(localDate, otherDate, frequency) && isDayOfMonth(otherDate, dayOfMonth)
}

class MonthlyRecurringOnEventTimeExpression(startingOn: Temporal, frequency: Int, unit: ChronoUnit, dayOfWeek: DayOfWeek, eventOccurrence: Event) extends TimeExpression {
  override def isRecurringOn(otherDate: LocalDate): Boolean = doesRecurMonthly(startingOn, otherDate, frequency) && isDayOfWeek(otherDate, dayOfWeek) && eventOccurrence.testEvent(otherDate)
}

class YearlyRecurringTimeExpression(fromYear: Int, frequency: Int, day: MonthDay) extends TimeExpression {
  override def isRecurringOn(otherDate: LocalDate): Boolean = doesRecurYearly(Year.of(fromYear), otherDate, frequency) && isDayOfMonth(otherDate, day.getDayOfMonth) && isMonthOfYear(otherDate, day.getMonthValue)
}

trait Event {
  def testEvent(from: LocalDate): Boolean
}

object FirstDayOfMonth extends Event {
  override def testEvent(localDate: LocalDate): Boolean = {
    val startOfMonth = YearMonth.of(localDate.getYear, localDate.getMonth).atDay(1)
    WEEKS.between(startOfMonth, localDate) == 0
  }
}

object LastDayOfWeek extends Event {
  override def testEvent(from: LocalDate): Boolean = {
    WEEKS.between(from, YearMonth.of(from.getYear, from.getMonth).atEndOfMonth()) == 0
  }
}

