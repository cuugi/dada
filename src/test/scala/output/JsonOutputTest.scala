package output

import org.scalatest.FunSuite
import dada.{Dimension, Sample, Activity}
import org.joda.time.{Duration, DateTime}
import dada.output.JsonOutput

class JsonOutputTest extends FunSuite {
  val startTime = DateTime.now

  def createSimple: Activity = {
    new Activity(startTime).
      addSample(new Sample[Number](0.54, Dimension.Distance, Duration.standardSeconds(1))).
      addSample(new Sample[Number](9.4, Dimension.Speed, Duration.standardSeconds(1))).
      addSample(new Sample[Number](150, Dimension.HeartRate, Duration.standardSeconds(1))).
      addSample(new Sample[Number](1402.22, Dimension.Altitude, Duration.standardSeconds(1))).
      addSample(new Sample[Number](60.1, Dimension.Latitude, Duration.standardSeconds(1))).
      addSample(new Sample[Number](20.1, Dimension.Longitude, Duration.standardSeconds(1)))
  }

  test("Initialize") {
    val activity = createSimple
    val output = new JsonOutput(activity)
    println(output)
  }
}
