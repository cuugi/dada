package dada

import scala.language.postfixOps
import scala.util.parsing.json.JSONObject

import com.github.nscala_time.time.Imports.DateTime
import com.github.nscala_time.time.Imports.Duration
import scala.annotation.tailrec

// Dimension enumeration - values in SI
object Dimension extends Enumeration {
  type Dimension = Value
  val Time, Distance, Speed, HeartRate, Altitude, Latitude, Longitude, Energy = Value
}

import Dimension._

// FigureType enumeration
object FigureType extends Enumeration {
  type FigureType = Value
  val Exact, Minimum, Average, Maximum = Value
}

import FigureType._

// Figure class
class Figure[T](v: T, dimension: Dimension, ftype: FigureType) {
  require(dimension != null)
  require(ftype != null)

  val value = v

  def this(v: T, dimension: Dimension) = this(v, dimension, Exact)

  def isDimension(d: Dimension) = (dimension == d)

  def isFigureType(ft: FigureType) = (ftype == ft)
}

// Sample class
class Sample[T](v: T, dimension: Dimension, t: Duration) {
  require(dimension != null)
  require(t != null)

  val value = v
  val time = t

  def isDimension(d: Dimension) = (dimension == d)

  override def toString(): String = "(" + value + ", " + dimension + ", " + time + ")"
}

// Activity class
class Activity(startTime: DateTime,
               figures: List[Figure[Number]],
               samples: List[Sample[Number]]) {
  require(startTime isBeforeNow)
  require(figures != null)
  require(samples != null)

  val id = startTime

  def this(startTime: DateTime) = this(startTime, Nil, Nil)

  private def sampleSort(s1: Sample[Number], s2: Sample[Number]): Boolean = (s1.time isLongerThan s2.time)

  /** Returns an activity with sorted samples. */
  def sorted = new Activity(startTime, figures, samples sortWith sampleSort reverse)

  /** Activity must be sorted for this to work. */
  def duration: Duration = samples.last.time

  /** Activity must be sorted for this to work. */
  def stopTime: DateTime = startTime plus duration

  def addFigure(figure: Figure[Number]) = new Activity(startTime, figure :: figures, samples)

  def addSample(sample: Sample[Number]) = new Activity(startTime, figures, sample :: samples)

  def addSamples(samples: List[Sample[Number]]) = new Activity(startTime, figures, samples ::: this.samples)

  def figureCount() = figures.length

  def sampleCount() = samples.length

  def getFigure(d: Dimension, ft: FigureType) = figures find ((f: Figure[Number]) => (f.isDimension(d) && f.isFigureType(ft)))

  def getFigure(d: Dimension): Option[Figure[Number]] = getFigure(d, Exact)

  def getSamples(d: Dimension) = samples filter (_.isDimension(d))

  private val zeroTime = Duration.standardSeconds(0)

  // TODO big interval values cause problems
  @tailrec
  private def nullIntervalledSamples(acc: List[Number], interval: Duration, time: Duration, samples: List[Sample[Number]]): List[Number] =
    if (time isShorterThan zeroTime)
      acc
    else {
      val take = (!samples.isEmpty) && ((time isEqual samples.head.time) || (time isShorterThan samples.head.time))
      val sample = take match {
        case true => samples.head.value
        case _ => null
      }
      val tail = take match {
        case true => samples.tail
        case _ => samples
      }
      nullIntervalledSamples(sample :: acc, interval, time minus interval, tail)
    }

  def getIntervalledSamples(d: Dimension, interval: Duration, duration: Duration): List[Number] = {
    val s = getSamples(d).sortWith(sampleSort)
    nullIntervalledSamples(Nil, interval, duration, s)
  }

  def toJson() = new JSONObject(Map("startTime" -> startTime))

  override def toString(): String = toJson.toString

}