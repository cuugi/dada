package dada.input

import scala.xml.{Node, XML}
import dada.{Dimension, Sample, Activity}
import org.joda.time.{Duration, DateTime}

class GpxInput(xmldoc: scala.xml.Elem) extends Input {
  require(xmldoc != null)

  def this(filename: String) = this(XML load filename)

  // TODO trait me
  private def calculateDuration(startTime: DateTime, trackPointNode: Node): Duration =
    (trackPointNode.find(_.label == "time")) match {
      case Some(node) => new Duration(startTime, new DateTime(node.head.text))
      case None => Duration.standardSeconds(0)
    }

  private def calculateDuration(startTime: DateTime, i: Int): Duration = Duration.standardSeconds(i)

  private def parseAltitudeSamples(startTime: DateTime, trakptNodes: Seq[Node]): Seq[Sample[Number]] =
    trakptNodes.zipWithIndex.map({
      case (trakptNode, i) => {
        val value = (trakptNode \ "ele").text.trim
        new Sample[Number](value match {
          case "" => null
          case _ => value.toDouble
        }, Dimension.Altitude, calculateDuration(startTime, i))
      }
    }).filter(_.value != null)

  private def parseLatitudeSamples(startTime: DateTime, trakptNodes: Seq[Node]): Seq[Sample[Number]] =
    trakptNodes.zipWithIndex.map({
      case (trakptNode, i) => {
        val value = (trakptNode \ "@lat").text.trim
        new Sample[Number](value match {
          case "" => null
          case _ => value.toDouble
        }, Dimension.Latitude, calculateDuration(startTime, i))
      }
    }).filter(_.value != null)

  private def parseLongitudeSamples(startTime: DateTime, trakptNodes: Seq[Node]): Seq[Sample[Number]] =
    trakptNodes.zipWithIndex.map({
      case (trakptNode, i) => {
        val value = (trakptNode \ "@lon").text.trim
        new Sample[Number](value match {
          case "" => null
          case _ => value.toDouble
        }, Dimension.Longitude, calculateDuration(startTime, i))
      }
    }).filter(_.value != null)

  private def parseRoute(trkNode: Node): Activity = {
    val startTime = new DateTime(trkNode.\("name").head.text) // TODO not a valid constraint
    val trakptNodes = (trkNode \\ "trkpt")

    val altitudeSamples = parseAltitudeSamples(startTime, trakptNodes).toList
    val latitudeSamples = parseLatitudeSamples(startTime, trakptNodes).toList
    val longitudeSamples = parseLongitudeSamples(startTime, trakptNodes).toList

    new Activity(startTime).
      addSamples(altitudeSamples).
      addSamples(latitudeSamples).
      addSamples(longitudeSamples)
  }

  override def toActivities: Seq[Activity] = {
    // TODO check schema versions
    (xmldoc \ "trk").map(parseRoute(_))
  }
}
