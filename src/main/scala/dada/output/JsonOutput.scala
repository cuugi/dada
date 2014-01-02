package dada.output

import dada._
import scala.util.parsing.json.{JSONFormat, JSONArray, JSONObject}
import org.joda.time.{DateTimeZone, Duration, DateTime}
import Dimension._
import scala.util.parsing.json.JSONArray
import scala.util.parsing.json.JSONObject
import dada.Dimension

class JsonOutput(activity: Activity) extends Output {
  require(activity != null)

  val sampleInterval = Duration.standardSeconds(5)

  private def toValue(value: Any): Any =
    value match {
      case null => "null"
      case o: Option[_] => toValue(o.get)
      case f: Figure[_] => toValue(f.value)
      case d: DateTime => d.toDateTime(DateTimeZone.UTC)
      case n: Number => n
      case _ => value.toString
    }

  private def samples(dimension: Dimension, interval: Duration): JSONObject =
    new JSONObject(Map(
      "interval" -> toValue(interval),
      "values" -> new JSONArray(activity.getIntervalledSamples(dimension, interval, activity.duration).map(toValue))
    ))

  private def statistics(dimension: Dimension): JSONObject =
    new JSONObject(Map(
      "minimum" -> activity.getFigure(dimension, FigureType.Minimum),
      "average" -> activity.getFigure(dimension, FigureType.Average),
      "maximum" -> activity.getFigure(dimension, FigureType.Maximum)
    ).filter(_._2.isDefined).mapValues(_.get.value))

  private def route: JSONObject = {
    val sorted = activity.sorted
    val latitudeSamples = sorted.getSamples(Dimension.Latitude)
    val longitudeSamples = sorted.getSamples(Dimension.Longitude)
    val route = latitudeSamples zip longitudeSamples
    val diffToStartTime = route.head._1.time
    new JSONObject(Map(
      "startTime" -> toValue(activity.id.plus(diffToStartTime).toLocalDateTime),
      "location" -> new JSONArray(route.map((point) =>
        new JSONObject(Map(
          "time" -> toValue(point._1.time minus diffToStartTime),
          "latitude" -> toValue(point._1.value),
          "longitude" -> toValue(point._2.value)
        ))))
    ))
  }

  def toJSONObject: JSONObject = {
    val msToKmh = 3.6
    new JSONObject(Map(
      "startTime" -> toValue(activity.id.toLocalDateTime),
      "stopTime" -> toValue(activity.stopTime.toLocalDateTime),
      "distance" -> toValue(activity.getFigure(Dimension.Distance, FigureType.Exact)),
      "kiloCalories" -> toValue(activity.getFigure(Dimension.Energy, FigureType.Exact)),
      "duration" -> toValue(activity.duration),
      "statistics" -> new JSONObject(Map(
        "heartRate" -> statistics(Dimension.HeartRate)
      )),
      "samples" -> new JSONObject(Map(
        "distance" -> samples(Dimension.Distance, sampleInterval),
        "speed" -> samples(Dimension.Speed, sampleInterval),
        "heartRate" -> samples(Dimension.HeartRate, sampleInterval),
        "altitude" -> samples(Dimension.Altitude, sampleInterval)
      )),
      "recordedRoute" -> route
    ))
  }

  override def toString: String = {
    toJSONObject.toString().
      replaceAll("\"null\"", "null").
      replaceAll("},", "},\n")
  }
}
