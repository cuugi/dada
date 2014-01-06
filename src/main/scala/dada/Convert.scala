package dada

import scala.reflect.io.{Path, File}
import dada.input.{Input, TcxInput}
import dada.output.JsonOutput
import dada.storage.Connect

object Convert extends App {
  require(args.size == 2)
  println("Data to data conversion tool v0.1.2")

  val username = args(0)
  val password = args(1)

  val connect = (new Connect).authenticate(username, password).get
  val activities = connect.list(15)
  val inputs: List[Input] = activities.map(connect.load(_))

  // connect -> output
  val outputDir = File("output").toDirectory

  outputDir.createDirectory(false, false)
  inputs.foreach((input) => {
    val activity = input.toActivities.head
    println("Converting activity: " + activity.id)
    val output = new JsonOutput(activity)
    val outputFile = File(Path(Seq[String](outputDir.path, activity.id + ".json")))
    outputFile.createFile(false)
    val outputStr = output.toString
    val writer = outputFile.bufferedWriter(false)
    writer.write(outputStr)
    writer.close
  })

  connect.client.shutdown
}
