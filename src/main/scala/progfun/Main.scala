package fr.esgi.al.progfun

import fr.esgi.al.progfun.io.{AppConfig, InputReader}
import scala.util.{Failure, Success, Try}
import java.io.{File, PrintWriter}
<<<<<<< HEAD
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
=======
import fr.esgi.al.progfun.domain.DonneesIncorectesException 
>>>>>>> 37e018088c7f15b4ce4bce89052174a4c329610d

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object Main extends App {
  val res = for {
    appConfig <- { AppConfig.build() }

    output <- InputReader
      .parseTasks(appConfig.inputFile)
      .map {
        case (limit, mowers) => appConfig.marshaller.write(limit, mowers)
      }

    result <- Try {
      val outstream = new PrintWriter(new File(appConfig.outputFile))
      outstream.write(output)
      outstream.close()
      println(output)
    }.recoverWith {
      case _: Throwable =>
        Failure(
          new DonneesIncorectesException("Could not write in output file")
        )
    }
  } yield result

  res match {
    case Failure(exception) => throw exception
    case Success(_)         => ()
  }
}
