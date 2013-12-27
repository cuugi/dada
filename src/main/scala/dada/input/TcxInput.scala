package dada.input

import dada.{Dimension, Sample, Activity}
import scala.xml.{Node, XML}
import org.joda.time.{Duration, DateTime}

class TcxInput(filename: String) extends Input {
  require(filename != null)

  val xmldoc = XML load filename

  private def calculateDuration(startTime: DateTime, trackPointNode: Node): Duration = {
    val time = new DateTime(trackPointNode.\("Time").head.text)
    new Duration(startTime, time)
  }

  private def parseHeartRateSamples(startTime: DateTime, trackPointNodes: Seq[Node]): Seq[Sample[Number]] = {
    trackPointNodes.map(trackPointNode => {
      val value = (trackPointNode \ "HeartRateBpm" \ "Value").text.trim
      val duration = calculateDuration(startTime, trackPointNode)
      new Sample[Number](value match {
        case "" => null
        case _ => value.toShort
      }, Dimension.HeartRate, duration)
    }).filter(_.value != null)
  }

  private def parseSpeedSamples(startTime: DateTime, trackPointNodes: Seq[Node]): Seq[Sample[Number]] = {
    trackPointNodes.map(trackPointNode => {
      val value = (trackPointNode \ "Extensions" \ "TPX" \ "Speed").text.trim
      val duration = calculateDuration(startTime, trackPointNode)
      new Sample[Number](value match {
        case "" => null
        case _ => value.toDouble
      }, Dimension.Speed, duration)
    }).filter(_.value != null)
  }

  private def parseDistanceSamples(startTime: DateTime, trackPointNodes: Seq[Node]): Seq[Sample[Number]] = {
    trackPointNodes.map(trackPointNode => {
      val value = (trackPointNode \ "DistanceMeters").text.trim
      val duration = calculateDuration(startTime, trackPointNode)
      new Sample[Number](value match {
        case "" => null
        case _ => value.toDouble
      }, Dimension.Distance, duration)
    }).filter(_.value != null)
  }

  private def parseAltitudeSamples(startTime: DateTime, trackPointNodes: Seq[Node]): Seq[Sample[Number]] = {
    trackPointNodes.map(trackPointNode => {
      val value = (trackPointNode \ "AltitudeMeters").text.trim
      val duration = calculateDuration(startTime, trackPointNode)
      new Sample[Number](value match {
        case "" => null
        case _ => value.toDouble
      }, Dimension.Altitude, duration)
    }).filter(_.value != null)
  }

  private def parseLatitudeSamples(startTime: DateTime, trackPointNodes: Seq[Node]): Seq[Sample[Number]] = {
    trackPointNodes.map(trackPointNode => {
      val value = (trackPointNode \ "Position" \ "LatitudeDegrees").text.trim
      val duration = calculateDuration(startTime, trackPointNode)
      new Sample[Number](value match {
        case "" => null
        case _ => value.toDouble
      }, Dimension.Latitude, duration)
    }).filter(_.value != null)
  }

  private def parseLongitudeSamples(startTime: DateTime, trackPointNodes: Seq[Node]): Seq[Sample[Number]] = {
    trackPointNodes.map(trackPointNode => {
      val value = (trackPointNode \ "Position" \ "LongitudeDegrees").text.trim
      val duration = calculateDuration(startTime, trackPointNode)
      new Sample[Number](value match {
        case "" => null
        case _ => value.toDouble
      }, Dimension.Longitude, duration)
    }).filter(_.value != null)
  }

  private def parseActivity(activityNode: Node): Activity = {
    val startTime = new DateTime(activityNode.\("Id").head.text)

    val trackPointNodes = (activityNode \\ "Trackpoint")
    val heartRateSamples = parseHeartRateSamples(startTime, trackPointNodes)
    val speedSamples = parseSpeedSamples(startTime, trackPointNodes)
    val distanceSamples = parseDistanceSamples(startTime, trackPointNodes)
    val altitudeSamples = parseAltitudeSamples(startTime, trackPointNodes)
    val latitudeSamples = parseLatitudeSamples(startTime, trackPointNodes)
    val longitudeSamples = parseLongitudeSamples(startTime, trackPointNodes)

    new Activity(startTime).
      addSamples(heartRateSamples.toList).
      addSamples(speedSamples.toList).
      addSamples(distanceSamples.toList).
      addSamples(altitudeSamples.toList).
      addSamples(latitudeSamples.toList).
      addSamples(longitudeSamples.toList)
  }

  override def toActivities: Seq[Activity] = {
    // TODO check schema versions
    (xmldoc \ "Activities" \ "Activity").map(parseActivity(_))
  }
}
