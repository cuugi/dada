package input

import org.scalatest.FunSuite
import dada.{Dimension, Activity}
import dada.input.GpxInput

class GpxInputTest extends FunSuite {

  test("test1.gpx") {
    val gpxInput = new GpxInput("src/test/input/test1.gpx")
    val activities: Seq[Activity] = gpxInput.toActivities
    activities.foreach(activity => println("Activity: " + activity))
    assert(activities.size === 1)
    assert(activities.head.getSamples(Dimension.HeartRate).size === 0)
    assert(activities.head.getSamples(Dimension.Altitude).head.value === 120.1)
    assert(activities.head.getSamples(Dimension.Latitude).head.value === 62.258805083110929)
    assert(activities.head.getSamples(Dimension.Longitude).head.value === 25.693706693127751)
    assert(activities.head.getSamples(Dimension.Latitude).length === 84)
    assert(activities.head.getSamples(Dimension.Longitude).length === 84)
  }

}