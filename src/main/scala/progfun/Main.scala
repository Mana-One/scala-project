package fr.esgi.al.progfun

import fr.esgi.al.progfun.io.{AppConfig, InputReader}
import scala.util.{Failure, Success, Try}
import java.io.{File, PrintWriter}
import fr.esgi.al.progfun.domain.DonneesIncorectesException 

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
