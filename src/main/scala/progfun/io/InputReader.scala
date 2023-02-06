package fr.esgi.al.progfun.io

// import scala.io.{Codec, Source}
import scala.util.{Failure, Success, Try}
import fr.esgi.al.progfun.domain.{Limit, LimitParser, Mower, MowerParser}
import better.files._
import fr.esgi.al.progfun.domain.DonneesIncorectesException

object InputReader {
  def parseTasks(filename: String): Try[(Limit, List[Mower])] = {
    val lines = {
      val file = File(filename)
      if (file.exists()) Success(file)
      else Failure(new DonneesIncorectesException("Input file not found"))
    }.map(f => f.lines.toList)

    lines match {
      case Failure(exception) => Failure(exception)
      case Success(ls) =>
        for {
          limit  <- LimitParser.parse(ls.headOption.getOrElse(""))
          mowers <- MowerParser.parseMany(ls.drop(1), limit)
        } yield (limit, mowers)
    }
  }
}
