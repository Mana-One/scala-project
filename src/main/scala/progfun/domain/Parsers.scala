package fr.esgi.al.progfun.domain

import scala.util.{Failure, Success, Try}

object IntParser {
  def parse (v: String): Try[Int] = Try(v.toInt)
    .recoverWith {
      case _: Throwable => Failure(new DonneesIncorectesException("Invalid integer value"))
    }
}

object LimitParser {
  private def checkAxis(axis: Int): Try[Int] = {
    if (axis <= 0) Failure(new DonneesIncorectesException("Zero or negative limit value."))
    else           Success(axis)
  }

  def parse(v: String): Try[Limit] = v.split(" ").toList match {
    case x :: y :: Nil =>
      for {
        cx <- IntParser.parse(x).flatMap(checkAxis)
        cy <- IntParser.parse(y).flatMap(checkAxis)
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

  def parseMany(inputs: String): Try[List[Instruction]] = inputs.toList
    .map(input => parse(input)) // List[Try[Instruction]]
    .foldLeft[Try[List[Instruction]]](Success(Nil)) { (acc, input) =>
      (acc, input) match {
        case (Failure(e), _)            => Failure(e)
        case (_, Failure(e))            => Failure(e)
        case (Success(arr), Success(i)) => Success(arr :+ i)
      }
    }
}

object DirectionParser {
  def parse(v: String): Try[Direction] = v match {
    case "N" => Success(North)
    case "E" => Success(East)
    case "W" => Success(West)
    case "S" => Success(South)
    case _   => Failure(new DonneesIncorectesException("Invalid direction"))
  }
}

object CoordinatesParser {
  private def checkCoordinate(coordinate: Int, axisLimit: Int): Try[Int] = {
    if (coordinate < 0 || axisLimit < coordinate) 
      Failure(new DonneesIncorectesException("Out of lawn boubds."))
    else
      Success(coordinate)
  }

  def parse(v: String, limit: Limit): Try[Coordinates] =
    v.split(" ").toList match {
      case x :: y :: d :: Nil =>
        for {
          cx <- IntParser.parse(x).flatMap(cx => checkCoordinate(cx, limit.x))
          cy <- IntParser.parse(y).flatMap(cy => checkCoordinate(cy, limit.y))
          cd <- DirectionParser.parse(d)
        } yield Coordinates(cx, cy, cd)
      case _ => Failure(new DonneesIncorectesException("Invalid coordinates"))
    }
}