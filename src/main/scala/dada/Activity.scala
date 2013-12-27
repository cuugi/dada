package dada

import scala.util.parsing.json.JSONObject

import com.github.nscala_time.time.Imports.DateTime
import com.github.nscala_time.time.Imports.Duration

// Dimension enumeration - values in SI
object Dimension extends Enumeration {
  type Dimension = Value
  val Time, Distance, Speed, HeartRate, Altitude, Latitude, Longitude = Value
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

  def addFigure(figure: Figure[Number]) = new Activity(startTime, figure :: figures, samples)

  def addSample(sample: Sample[Number]) = new Activity(startTime, figures, sample :: samples)

  def addSamples(samples: List[Sample[Number]]) = new Activity(startTime, figures, samples ::: this.samples)

  def figureCount() = figures.length

  def sampleCount() = samples.length

  def getFigure(d: Dimension, ft: FigureType) = figures find ((f: Figure[Number]) => (f.isDimension(d) && f.isFigureType(ft)))

  def getFigure(d: Dimension): Option[Figure[Number]] = getFigure(d, Exact)

  def getSamples(d: Dimension) = samples filter (_.isDimension(d))

  private val zeroTime = new Duration(0)

  private def nullIntervalledSamples(interval: Duration, time: Duration, samples: List[Sample[Number]]): List[Number] =
    if (time isShorterThan zeroTime)
      Nil
    else if (samples isEmpty)
      null :: nullIntervalledSamples(interval, time minus interval, samples)
    else if (time isEqual samples.head.time)
      samples.head.value :: nullIntervalledSamples(interval, time minus interval, samples.tail)
    else if (time isShorterThan samples.head.time)
      samples.head.value :: nullIntervalledSamples(interval, time minus interval, samples.tail)
    else
      null :: nullIntervalledSamples(interval, time minus interval, samples)

  def getIntervalledSamples(d: Dimension, interval: Duration, duration: Duration): List[Number] = {
    val s = getSamples(d).sortWith((s1: Sample[Number], s2: Sample[Number]) => (s1.time isLongerThan s2.time))
    nullIntervalledSamples(interval, duration, s).reverse
  }

  def toJson() = new JSONObject(Map("startTime" -> startTime))

  override def toString(): String = toJson.toString

}