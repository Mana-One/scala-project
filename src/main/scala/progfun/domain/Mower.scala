package fr.esgi.al.progfun.domain

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
object North extends Direction
object East extends Direction
object West extends Direction
object South extends Direction

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

  def applyInstruction(instruction: Instruction, limit: Limit): Coordinates =
    (this, instruction) match {
      case (Coordinates(x, y, d), RotateLeft) =>
        Coordinates(x, y, d.rotateToTheLeft())
      case (Coordinates(x, y, d), RotateRight) =>
        Coordinates(x, y, d.rotateToTheRight())
      case (_, Advance) => this.moveForward(limit)
    }
}

final case class Mower(
    start: Coordinates,
    instructions: List[Instruction],
    limit: Limit
) {
  def run(): Coordinates = instructions.foldLeft(start) { (dest, instruction) =>
    dest.applyInstruction(instruction, limit)
  }
}
