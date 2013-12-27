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

  private def parseActivity(activityNode: Node): Activity = {
    val startTime = new DateTime(activityNode.\("Id").head.text)

    val trackPointNodes = (activityNode \\ "Trackpoint")
    val heartRateSamples = parseHeartRateSamples(startTime, trackPointNodes)

    new Activity(startTime).addSamples(heartRateSamples.toList)
  }

  override def toActivities: Seq[Activity] = {
    // TODO check versions
    (xmldoc \ "Activities" \ "Activity").map(parseActivity(_))
  }
}
