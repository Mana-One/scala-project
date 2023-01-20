package fr.esgi.al.progfun.domain

import scala.util.{Failure, Success, Try}


final case class Limit(x: Int, y: Int)

sealed trait Instruction
object Advance extends Instruction
object RotateLeft extends Instruction
object RotateRight extends Instruction

sealed abstract class Direction(val value: String) {
  override def toString(): String = value

  def rotateToTheLeft(): Direction = this match {
    case North2 => West2
    case East2  => North2
    case West2  => South2
    case South2 => East2
  }

  def rotateToTheRight(): Direction = this match {
    case North2 => East2
    case East2  => South2
    case West2  => North2
    case South2 => West2
  }
}
object North2 extends Direction("N")
object East2 extends Direction("E")
object West2 extends Direction("W")
object South2 extends Direction("S")

final case class Coordinates(x: Int, y: Int, direction: Direction) {
  private def moveForward(edges: Limit): Coordinates = this match {
    case Coordinates(x, y, North2) if (y + 1 <= edges.y) =>
      Coordinates(x, y + 1, North2)
    case Coordinates(x, y, West2) if (0 <= x - 1) =>
      Coordinates(x - 1, y, West2)
    case Coordinates(x, y, East2) if (x + 1 <= edges.x) =>
      Coordinates(x + 1, y, East2)
    case Coordinates(x, y, South2) if (0 <= y - 1) =>
      Coordinates(x, y - 1, South2)
    case _ => this
  }

  def followInstruction(
      instruction: Instruction,
      edges: Limit
  ): Coordinates = (this, instruction) match {
    case (Coordinates(x, y, d), RotateLeft) =>
      Coordinates(x, y, d.rotateToTheLeft())
    case (Coordinates(x, y, d), RotateRight) =>
      Coordinates(x, y, d.rotateToTheRight())
    case (_, Advance) => this.moveForward(edges)
  }
}

object Coordinates {
  private def parseInt(v: String, limit: Int): Try[Int] =
    v.toIntOption
      .flatMap(x => if (0 <= x && x <= limit) Some(x) else None)
      .fold[Try[Int]](
        Failure(new DonneesIncorectesException("Invalid coordinate value"))
      )(Success.apply)

  private def parseDirection(v: String): Try[Direction] = v match {
    case "N" => Success(North2)
    case "E" => Success(East2)
    case "W" => Success(West2)
    case "S" => Success(South2)
    case _   => Failure(new DonneesIncorectesException("Invalid direction"))
  }

  def parse(v: String, edges: Limit): Try[Coordinates] =
    v.split(" ").toList match {
      case x :: y :: d :: Nil =>
        for {
          cx <- parseInt(x, edges.x)
          cy <- parseInt(y, edges.y)
          cd <- parseDirection(d)
        } yield Coordinates(cx, cy, cd)
      case _ => Failure(new DonneesIncorectesException("Invalid coordinates"))
    }
}

final case class Mower(
    start: Coordinates,
    instructions: List[Instruction],
    worldEdges: Limit
) {
  def run(): Coordinates =
    instructions
      .foldLeft(start) { (dest, instruction) =>
        dest.followInstruction(instruction, worldEdges)
      }
}

object Mower {
  def parse(
      coordinates: String,
      instructions: String,
      edges: Limit
  ): Try[Mower] =
    for {
      c <- Coordinates.parse(coordinates, edges)
      i <- InstructionParser.parseMany(instructions)
    } yield Mower(c, i, edges)

  def parseMany(
      inputs: List[String],
      edges: Limit
  ): Try[List[Mower]] = inputs match {
    case _ :: Nil =>
      Failure(new DonneesIncorectesException("Invalid mowers input"))
    case coordinates :: instructions :: next =>
      for {
        mower <- parse(coordinates, instructions, edges)
        rest  <- parseMany(next, edges)
      } yield mower :: rest
    case Nil => Success(Nil)
  }
}
