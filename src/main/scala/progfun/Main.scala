package fr.esgi.al.progfun

import fr.esgi.al.progfun.io.{AppConfig, InputReader, OutputWriter}
import scala.util.{Failure, Success}

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object Main extends App {
  val res = for {
    appConfig <- { AppConfig.build() }

    output <- InputReader
      .parseTasks(appConfig.inputFile)
      .map {
        case (limit, mowers) => appConfig.marshaller.write(limit, mowers)
      }
  } yield OutputWriter.write(appConfig, output)

  res match {
    case Failure(exception) => throw exception
    case Success(_)         => ()
  }
}