package fr.esgi.al.progfun

import fr.esgi.al.progfun.io._
import scala.util.{Failure, Success}
import java.io.{File, PrintWriter}


@SuppressWarnings(Array("org.wartremover.warts.Throw"))
object Main extends App {
  val res = InputReader
    .parseTasks("test.txt")
    .map { case (limit, mowers) => JsonMarshaller.write(limit, mowers) }

  res match {
    case Failure(exception) => throw exception
    case Success(s) => {
      val outstream = new PrintWriter(new File("out.json"))
      outstream.write(s)
      outstream.close()
      println(s)
    }
  }
}
