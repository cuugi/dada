package dada.input

import dada.Activity

trait Input {
  def toActivities: Seq[Activity]
}
