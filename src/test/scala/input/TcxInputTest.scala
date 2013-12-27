package input

import org.scalatest.FunSuite
import dada.input.TcxInput
import dada.{Dimension, Activity}

class TcxInputTest extends FunSuite {
  test("test1.tcx") {
    val tcxInput = new TcxInput("src/test/input/test1.tcx")
    val activities: Seq[Activity] = tcxInput.toActivities
    activities.foreach(activity => println("Activity: " + activity))
    assert(activities.size === 1)
    assert(activities.head.getSamples(Dimension.HeartRate).size === 0)
  }

  test("test2.tcx") {
    val tcxInput = new TcxInput("src/test/input/test2.tcx")
    val activities: Seq[Activity] = tcxInput.toActivities
    activities.foreach(activity => println("Activity: " + activity))
    assert(activities.size === 1)
    assert(activities.head.getSamples(Dimension.HeartRate).size > 0)
    assert(activities.head.getSamples(Dimension.Speed).size > 0)
    assert(activities.head.getSamples(Dimension.Distance).size > 0)
    assert(activities.head.getSamples(Dimension.Altitude).size > 0)
    assert(activities.head.getSamples(Dimension.Latitude).size > 0)
    assert(activities.head.getSamples(Dimension.Longitude).size > 0)
  }

}
