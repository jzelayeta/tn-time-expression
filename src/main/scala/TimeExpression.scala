import java.time.temporal.ChronoUnit.{DAYS, MONTHS, WEEKS, YEARS}
import java.time.temporal.{ChronoUnit, Temporal, TemporalUnit}
import java.time._

object TimeExpression {

  /**
    * This expression matches on the date of parameter value.
    *
    * @param localDate a local date
    * @return a TimeExpression
    */
  def apply(localDate: LocalDate): TimeExpression = new NonRecurringTimeExpression(localDate)

  def daily(every: Int, from: LocalDate): TimeExpression = new DailyRecurringTimeExpression(from, every)

  def monthlyEvery(every: Int, dayOfMonth: Int, from: YearMonth): TimeExpression = new MonthlyRecurringOnDayOfMonthTimeExpression(from, every, dayOfMonth)

  def monthlyEvery(every: Int, dayOfWeek: DayOfWeek, eventOccurrence: EventOccurrence, from: YearMonth): TimeExpression = new MonthlyRecuringOnEventTimeExpression(from, every, MONTHS, dayOfWeek, eventOccurrence)

  def yearlyEvery(amountOfYears: Int, day: MonthDay, fromYear: Int): TimeExpression = new YearlyRecurringTimeExpression(amountOfYears, amountOfYears, day)

}

trait FrecuencyCondition {
  def doesReccurrOnFrecuency(from: Temporal, to: Temporal, frecuency: Int, unit: ChronoUnit): Boolean = unit.between(from, to) % frecuency == 0
}

abstract class TimeExpression {
  def isRecurringOn(otherDate: LocalDate): Boolean
}

trait DayOfMonthCondition {
  def isDayOfMonth(date: LocalDate, dayOfMonth: Int): Boolean = date.getDayOfMonth == dayOfMonth
}

trait DayOfWeekCondition {
  def isDayOfWeek(date: LocalDate, dayOfWeek: DayOfWeek): Boolean = date.getDayOfWeek == dayOfWeek
}

trait LastDayOfWeekCondition extends EventOccurrence {
  def isLastOf(from: LocalDate) = WEEKS.between(from, YearMonth.of(from.getYear, from.getMonth).atEndOfMonth()) == 0
}


class NonRecurringTimeExpression(localDate: LocalDate) extends TimeExpression {
  override def isRecurringOn(otherDate: LocalDate): Boolean = localDate == otherDate
}

class DailyRecurringTimeExpression(localDate: LocalDate, frecuency: Int) extends TimeExpression with FrecuencyCondition {
  override def isRecurringOn(otherDate: LocalDate): Boolean = doesReccurrOnFrecuency(localDate, otherDate, frecuency, DAYS)
}

class MonthlyRecurringOnDayOfMonthTimeExpression(localDate: Temporal, frecuency: Int, dayOfMonth: Int) extends TimeExpression with FrecuencyCondition with DayOfMonthCondition {
  override def isRecurringOn(otherDate: LocalDate): Boolean = doesReccurrOnFrecuency(localDate, otherDate, frecuency, MONTHS) && isDayOfMonth(otherDate, dayOfMonth)
}

class MonthlyRecuringOnEventTimeExpression(startingOn: Temporal, frecuency: Int, unit: ChronoUnit, dayOfWeek: DayOfWeek, eventOccurrence: EventOccurrence) extends TimeExpression with FrecuencyCondition with DayOfWeekCondition {
  override def isRecurringOn(otherDate: LocalDate): Boolean = doesReccurrOnFrecuency(startingOn, otherDate, frecuency, MONTHS) && isDayOfWeek(otherDate, dayOfWeek) && eventOccurrence.testEvent(otherDate)
}

class YearlyRecurringTimeExpression(fromYear: Int, frecuency: Int, day: MonthDay) extends TimeExpression with FrecuencyCondition {
  override def isRecurringOn(otherDate: LocalDate): Boolean = doesReccurrOnFrecuency(Year.of(fromYear),otherDate, frecuency, YEARS)
}

trait EventOccurrence {
  def testEvent(from: LocalDate): Boolean
}

object FirstDayOfMonth extends EventOccurrence {
  override def testEvent(localDate: LocalDate): Boolean = {
    val startOfMonth = YearMonth.of(localDate.getYear, localDate.getMonth).atDay(1)
    WEEKS.between(startOfMonth, localDate) == 0
  }
}

object LastDayOfWeek extends EventOccurrence {
  override def testEvent(from: LocalDate): Boolean = {
    WEEKS.between(from, YearMonth.of(from.getYear, from.getMonth).atEndOfMonth()) == 0
  }
}

