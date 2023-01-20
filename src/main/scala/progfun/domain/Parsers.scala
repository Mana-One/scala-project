package fr.esgi.al.progfun.domain

import scala.util.{Failure, Success, Try}

object IntParser {
  def parse (v: String): Try[Int] = Try(v.toInt)
    .flatMap(x => {
      if (x >= 0) Success(x) 
      else        Failure(new DonneesIncorectesException("Invalid limit"))
    })
    .recoverWith {
      case _: Throwable => Failure(new DonneesIncorectesException("Invalid limit"))
    }
}

object LimitParser {
  def parse(v: String): Try[Limit] = v.split(" ").toList match {
    case x :: y :: Nil =>
      for {
        cx <- IntParser.parse(x)
        cy <- IntParser.parse(y)
      } yield Limit(cx, cy)
    case _ => Failure(new DonneesIncorectesException("Invalid limits"))
  }
}

object InstructionParser {
  private def parse(input: Char): Try[Instruction] = input match {
    case 'G' => Success(RotateLeft)
    case 'D' => Success(RotateRight)
    case 'A' => Success(Advance)
    case _   => Failure(new DonneesIncorectesException("Invalid instruction"))
  }

  def parseMany(inputs: String): Try[List[Instruction]] = {
    inputs.toList
      .map(input => parse(input)) // List[Try[Instruction]]
      .foldLeft[Try[List[Instruction]]](Success(Nil)) { (acc, input) =>
        (acc, input) match {
          case (Failure(e), _)            => Failure(e)
          case (_, Failure(e))            => Failure(e)
          case (Success(arr), Success(i)) => Success(arr :+ i)
        }
      }
  }
}