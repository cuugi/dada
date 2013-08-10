package dada

import scala.util.parsing.json.JSONObject

import com.github.nscala_time.time.Imports.DateTime
import com.github.nscala_time.time.Imports.Duration

// Dimension enumeration - values in SI
object Dimension extends Enumeration {
   type Dimension = Value
   val Time, Distance, Speed, HeartRate = Value
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
class Sample[T](value: T, dimension: Dimension, time: Duration) {
	def isDimension(d: Dimension) = (dimension == d)
}

// Activity class
class Activity(
	startTime: DateTime, 
	figures: List[Figure[Number]], 
	samples: List[Sample[Number]]) {
	require(startTime isBeforeNow)
	require(figures != null)
	require(samples != null)

	val id = startTime

	def this(startTime: DateTime) = this(startTime, Nil, Nil)

	def addFigure(figure: Figure[Number]) = new Activity(startTime, figure :: figures, samples)
	def addSample(sample: Sample[Number]) = new Activity(startTime, figures, sample :: samples)

	def figureCount() = figures.length
	def sampleCount() = samples.length

  def getFigure(d: Dimension, ft: FigureType) = figures find((f: Figure[Number]) => (f.isDimension(d) && f.isFigureType(ft)))
  def getFigure(d: Dimension): Option[Figure[Number]] = getFigure(d, Exact)
  def getSamples(d: Dimension) = samples filter(_.isDimension(d))

	def toJson() = new JSONObject(Map("startTime" -> startTime))
	override def toString(): String = toJson.toString 

}