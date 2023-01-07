package fr.esgi.al.progfun

import scala.io.Source
import fr.esgi.al.progfun.domain.{Limit, Mower}
import scala.io.Codec
import scala.util.{Failure, Success}

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object Main extends App {
  println("Ici le programme principal")
  // Le code suivant ne compilera pas.
  // var tmp = null;
  // var tmp2 = if (tmp == 1) "yes" else 1

  // println(s"tmp: $tmp, tmp2: $tmp2")
  val buffer = Source.fromFile("test.txt")(Codec.UTF8)
  val lines = buffer.getLines().toList
  buffer.close()

  // val res = for {
  //   d  <- Direction.parse(lines(0))
  //   p  <- Point.parse(lines(1))
  //   is <- Instruction.parseMany(lines(2).toList)
  // } yield (d, p, is)

  val res = for {
    limit  <- Limit.parse(lines.headOption.getOrElse(""))
    mowers <- Mower.parseMany(lines.drop(1), limit)
  } yield (limit, mowers)

  res match {
    case Failure(exception) => throw exception
    case Success((limit, mowers)) => {
      println(limit)
      mowers.foreach(
        mower => println(s"${mower.toString()} | ${mower.run().toString()}")
      )
    }
  }
  ()
}
