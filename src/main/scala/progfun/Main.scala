package fr.esgi.al.progfun

import fr.esgi.al.progfun.io._
import scala.util.{Failure, Success}
import java.io.{File, PrintWriter}
import com.typesafe.config.{Config, ConfigFactory}
import scala.util.Try

case class AppConfig(outputType: String, inputFile: String) 

object AppConfigProps  {
  val conf: Config = ConfigFactory.load()

  def getOutputType: Try[String] = Try(conf.getString("application.output-type")) 

  def getInputFile: Try[String] = Try(conf.getString("application.input-file")) 

  def getMarshaller(outputType: String): Try[Marshaller] = outputType match {
    case "json" => Success(JsonMarshaller)
    case "csv" => Success(CSVMarshaller)
    case "yaml" => Success(YamlMarshaller)
    case _ => Failure(new Exception("Invalid output type"))
  }
  
  def getFileName(outputType: String): Try[String] = outputType match {
    case "json" => Success(conf.getString("application.output-json-file"))
    case "csv" => Success(conf.getString("application.output-csv-file"))
    case "yaml" => Success(conf.getString("application.output-yaml-file"))
    case _ => Failure(new Exception("Invalid output type"))
  }
}

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object Main extends App {

  val outputType = AppConfigProps.getOutputType match {
    case Failure(exception) => throw exception
    case Success(outputType) => outputType
  }

  val inputFile = AppConfigProps.getInputFile match {
    case Failure(exception) => throw exception
    case Success(inputFile) => inputFile
  }

  val config: AppConfig = AppConfig(outputType ,inputFile)
  
  AppConfigProps.getMarshaller(config.outputType) match {
    case Failure(exception) => throw exception
    case Success(marshaller) => {
      val res = InputReader
        .parseTasks(config.inputFile)
        .map { case (limit, mowers) => marshaller.write(limit, mowers) }

      res match {
        case Failure(exception) => throw exception
        case Success(s) => {
          AppConfigProps.getFileName(config.outputType) match {
            case Failure(exception) => throw exception
            case Success(fileName) => {
              val outstream = new PrintWriter(new File(fileName))
              outstream.write(s)
              outstream.close()
              println(s)
            }
          }
        }
      }
    }
  }

}
