package fr.esgi.al.progfun.io

import scala.io.{Codec, Source}
import scala.util.Try
import fr.esgi.al.progfun.domain.{Limit, Mower}

object InputReader {
	def parseTasks(filename: String): Try[(Tuple2[Int, Int], List[Mower])] = {
		val lines = {
			val buffer = Source.fromFile(filename)(Codec.UTF8)
			val res = buffer.getLines().toList
			buffer.close()
			res
		}

		for {
				limit  <- Limit.parse(lines.headOption.getOrElse(""))
				mowers <- Mower.parseMany(lines.drop(1), limit)
		} yield (limit, mowers)
	}
}