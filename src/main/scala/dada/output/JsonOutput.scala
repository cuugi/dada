package dada.output

import dada.{Sample, Dimension, Activity}
import scala.util.parsing.json.{JSONFormat, JSONArray, JSONObject}
import org.joda.time.{Duration, DateTime}
import Dimension._

class JsonOutput(activity: Activity) extends Output {
  require(activity != null)

  val sampleInterval = Duration.standardSeconds(1)

  private def toJSONString(number: Number): String =
    number match {
      case null => "null"
      case _ => number.toString
    }

  private def samples(dimension: Dimension, interval: Duration): JSONObject =
    new JSONObject(Map(
      "interval" -> interval,
      "values" -> new JSONArray(activity.getIntervalledSamples(dimension, interval, activity.duration).map(toJSONString))
    ))

  private def route: JSONObject = {
    val sorted = activity.sorted
    val latitudeSamples = sorted.getSamples(Dimension.Latitude)
    val longitudeSamples = sorted.getSamples(Dimension.Longitude)
    val route = latitudeSamples zip longitudeSamples
    new JSONObject(Map(
      "startTime" -> activity.id.plus(route.head._1.time),
      "location" -> new JSONArray(route.map((point) =>
        new JSONObject(Map(
          "time" -> point._1.time,
          "latitude" -> point._1.value,
          "longitude" -> point._2.value
        ))))
    ))
  }

  override def toString: String = {
    new JSONObject(Map(
      "startTime" -> activity.id,
      "stopTime" -> activity.stopTime,
      "duration" -> activity.duration,
      "samples" -> new JSONObject(Map(
        "distance" -> samples(Dimension.Distance, sampleInterval),
        "speed" -> samples(Dimension.Speed, sampleInterval),
        "heartRate" -> samples(Dimension.HeartRate, sampleInterval),
        "altitude" -> samples(Dimension.Altitude, sampleInterval)
      )),
      "recordedRoute" -> route
    )).toString
  }
}
