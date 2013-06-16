package dada

import scala.util.parsing.json.JSONObject

import com.github.nscala_time.time.Imports.DateTime
import com.github.nscala_time.time.Imports.Duration

// Dimenstion enumeration - values in SI
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
class Figure[T](value: T, dimension: Dimension, ftype: FigureType) {
	def this(value: T, dimension: Dimension) = this(value, dimension, Exact)
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

	def toJson() = new JSONObject(Map("startTime" -> startTime))
	override def toString(): String = toJson.toString 

}