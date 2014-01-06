package dada

import scala.reflect.io.{Path, File}
import dada.input.{Input, TcxInput}
import dada.output.JsonOutput
import dada.storage.Connect

object Convert extends App {
  require(args.size == 2)
  println("Data to data conversion tool v0.1")

  val username = args(0)
  val password = args(1)

  val connect = new Connect
  val activities = connect.authenticate(username, password).get.listActivities(5)

  // input -> output
  val inputDir = File("input").toDirectory
  val outputDir = File("output").toDirectory

  val TcxPattern = """(.*)\.tcx$""".r
  val inputs: Iterator[Input] = inputDir.files.map((file) => {
    file.name match {
      case TcxPattern(filename) => new TcxInput("input/" + filename + ".tcx")
      case _ => null
    }
  }).filter(_ != null)

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
}
