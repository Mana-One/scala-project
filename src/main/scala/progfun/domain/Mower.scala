package fr.esgi.al.progfun.domain

import scala.util.{Failure, Success, Try}


final case class Limit(x: Int, y: Int)

sealed trait Instruction
object Advance extends Instruction
object RotateLeft extends Instruction
object RotateRight extends Instruction

sealed trait Direction {
  def rotateToTheLeft(): Direction = this match {
    case North => West
    case East  => North
    case West  => South
    case South => East
  }

  def rotateToTheRight(): Direction = this match {
    case North => East
    case East  => South
    case West  => North
    case South => West
  }
}
object North  extends Direction
object East   extends Direction
object West   extends Direction
object South  extends Direction

final case class Coordinates(x: Int, y: Int, direction: Direction) {
  private def moveForward(edges: Limit): Coordinates = this match {
    case Coordinates(x, y, North) if (y + 1 <= edges.y) =>
      Coordinates(x, y + 1, North)
    case Coordinates(x, y, West) if (0 <= x - 1) =>
      Coordinates(x - 1, y, West)
    case Coordinates(x, y, East) if (x + 1 <= edges.x) =>
      Coordinates(x + 1, y, East)
    case Coordinates(x, y, South) if (0 <= y - 1) =>
      Coordinates(x, y - 1, South)
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

  def parse(v: String, edges: Limit): Try[Coordinates] =
    v.split(" ").toList match {
      case x :: y :: d :: Nil =>
        for {
          cx <- parseInt(x, edges.x)
          cy <- parseInt(y, edges.y)
          cd <- DirectionParser.parse(d)
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
