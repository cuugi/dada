import dada._

import scala.language.postfixOps

import com.github.nscala_time.time.Imports.DateTime
import com.github.nscala_time.time.Imports.Duration

import org.scalatest.FunSuite

class ActivityTest extends FunSuite {
  test("Create activity") {
    val startTime = DateTime.now minusMinutes 10
    val activity = new Activity(startTime)
    assert(activity.id === startTime)
  }

  test("Add duration") {
    val activity = new Activity(DateTime.now minusDays 10)
    val activityWithDuration = activity.addFigure(new Figure(360, Dimension.Time))
    assert(activity.figureCount === 0)
    assert(activityWithDuration.figureCount === 1)
    assert(activityWithDuration.sampleCount === 0)
    assert(activity.getFigure(Dimension.Time) isEmpty)
    assert(!activityWithDuration.getFigure(Dimension.Time).isEmpty)
    assert(activityWithDuration.getFigure(Dimension.Time).get.value === 360)
  }

  test("Add speed samples") {
    val startTime = DateTime.now minusDays 10
    val activity = new Activity(startTime)
    val activityWithSamples = activity.
      addSample(new Sample(1, Dimension.Speed, new Duration(0))).
      addSample(new Sample(2, Dimension.Speed, new Duration(1000))).
      addSample(new Sample(3, Dimension.Speed, new Duration(2000))).
      addSample(new Sample(5, Dimension.Speed, new Duration(4000))).
      addSample(new Sample(4, Dimension.Speed, new Duration(3000))).sorted
    assert(activityWithSamples.figureCount === 0)
    assert(activityWithSamples.sampleCount === 5)
    assert(activityWithSamples.stopTime === (startTime plus new Duration(4000)))
    assert(activityWithSamples.getSamples(Dimension.Speed).length === 5)
    assert(activityWithSamples.getSamples(Dimension.HeartRate).length === 0)
  }

  test("Add a list of samples") {
    val activity = new Activity(DateTime.now minusDays 10)
    val activityWithSamples =
      activity.addSample(new Sample(1, Dimension.Speed, new Duration(0))).
        addSamples(List(new Sample(2, Dimension.Speed, new Duration(1000)),
        new Sample(3, Dimension.Speed, new Duration(2000)),
        new Sample(4, Dimension.Speed, new Duration(3000)),
        new Sample(5, Dimension.Speed, new Duration(4000))));
    assert(activityWithSamples.figureCount === 0)
    assert(activityWithSamples.sampleCount === 5)
    assert(activityWithSamples.getSamples(Dimension.Speed).length === 5)
    assert(activityWithSamples.getSamples(Dimension.HeartRate).length === 0)
  }

  test("Get intervalled samples") {
    val activity =
      new Activity(DateTime.now minusDays 10).
        addSample(new Sample(3, Dimension.Speed, new Duration(2000))).
        addSample(new Sample(5, Dimension.Speed, new Duration(5000)))
    assert(activity.sampleCount === 2)

    val intervalledSamples =
      activity.getIntervalledSamples(Dimension.Speed, new Duration(1000), new Duration(8000))
    assert(intervalledSamples != null)
    assert(intervalledSamples.length === 9)
    assert(intervalledSamples(0) == null)
    assert(intervalledSamples(2) === 3)
    assert(intervalledSamples(5) === 5)
  }
}