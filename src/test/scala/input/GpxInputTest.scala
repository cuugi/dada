package input

import org.scalatest.FunSuite
import dada.{Dimension, Activity}
import dada.input.GpxInput
import org.joda.time.Duration

class GpxInputTest extends FunSuite {

  test("test1.gpx") {
    val gpxInput = new GpxInput("src/test/input/test1.gpx")
    val activities: Seq[Activity] = gpxInput.toActivities
    activities.foreach(activity => println("Activity: " + activity))

    assert(activities.size === 1)
    val activity = activities.head.sorted
    assert(activity.getSamples(Dimension.HeartRate).size === 0)
    assert(activity.getSamples(Dimension.Altitude).head.value === 120.1)
    assert(activity.getSamples(Dimension.Latitude).head.value === 62.258805083110929)
    assert(activity.getSamples(Dimension.Longitude).head.value === 25.693706693127751)
    assert(activity.duration === Duration.standardSeconds(99))

    assert(activity.getIntervalledSamples(Dimension.Altitude,
      Duration.standardSeconds(1), activity.duration).length === 100)
    assert(activities.head.getSamples(Dimension.Latitude).length === 100)
    assert(activities.head.getSamples(Dimension.Longitude).length === 100)
  }

}
