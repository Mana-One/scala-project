package fr.esgi.al.progfun.io

import com.typesafe.config.{Config, ConfigFactory}
import scala.util.{Failure, Success, Try}
import fr.esgi.al.progfun.domain.DonneesIncorectesException

sealed trait OutputType
object JsonOutput extends OutputType
object CsvOutput extends OutputType
object YamlOutput extends OutputType

final case class AppConfig(
    inputFile: String,
    outputFile: String,
    marshaller: Marshaller
)
object AppConfig {
  private def getOutputType(str: String): Try[OutputType] = str match {
    case "json" => Success(JsonOutput)
    case "csv"  => Success(CsvOutput)
    case "yaml" => Success(YamlOutput)
    case _      => Failure(new DonneesIncorectesException("Invalid output type"))
  }

  private def getMarshaller(outputType: OutputType): Try[Marshaller] =
    outputType match {
      case JsonOutput => Success(JsonMarshaller)
      case CsvOutput  => Success(CSVMarshaller)
      case YamlOutput => Success(YamlMarshaller)
    }

  def getOutputFile(config: Config, outputType: OutputType): Try[String] =
    outputType match {
      case JsonOutput =>
        Try(config.getString("application.output-json-file"))
          .recoverWith {
            case _: Throwable =>
              Failure(
                new DonneesIncorectesException("Unreachable output file name")
              )
          }

      case CsvOutput =>
        Try(config.getString("application.output-csv-file"))
          .recoverWith {
            case _: Throwable =>
              Failure(
                new DonneesIncorectesException("Unreachable output file name")
              )
          }

      case YamlOutput =>
        Try(config.getString("application.output-yaml-file"))
          .recoverWith {
            case _: Throwable =>
              Failure(
                new DonneesIncorectesException("Unreachable output file name")
              )
          }
    }

  def build(): Try[AppConfig] = {
    val config = ConfigFactory.load()
    for {
      inputFile <- Try(config.getString("application.input-file"))
        .recoverWith {
          case _: Throwable =>
            Failure(
              new DonneesIncorectesException("Unreachable input file name")
            )
        }
      outputType <- Try(config.getString("application.output-type"))
        .recoverWith {
          case _: Throwable =>
            Failure(new DonneesIncorectesException("Unreachable output type"))
        }
        .flatMap(getOutputType)

      outputFile <- getOutputFile(config, outputType)
      marshaller <- getMarshaller(outputType)
    } yield AppConfig(inputFile, outputFile, marshaller)
  }
}
