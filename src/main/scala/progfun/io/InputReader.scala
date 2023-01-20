package fr.esgi.al.progfun.io

import scala.io.{Codec, Source}
import scala.util.Try
import fr.esgi.al.progfun.domain.{Limit, LimitParser, Mower}

object InputReader {
	def parseTasks(filename: String): Try[(Limit, List[Mower])] = {
		val lines = {
			val buffer = Source.fromFile(filename)(Codec.UTF8)
			val res = buffer.getLines().toList
			buffer.close()
			res
		}

		for {
				limit  <- LimitParser.parse(lines.headOption.getOrElse(""))
				mowers <- Mower.parseMany(lines.drop(1), limit)
		} yield (limit, mowers)
	}
}