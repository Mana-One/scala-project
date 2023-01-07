package fr.esgi.al.progfun.domain

import scala.util.{Try, Success, Failure}

object Limit {
	private def parseInt(v: String): Try[Int] = v.toIntOption
		.flatMap(x => if (x >= 0) Some(x) else None)
		.fold[Try[Int]](
			Failure(new DonneesIncorectesException("Invalid limit"))
		)(Success.apply)

	def parse(v: String): Try[Tuple2[Int, Int]] = v.split(" ").toList match {
		case x::y::Nil 			=> for {
			cx <- parseInt(x)
			cy <- parseInt(y)
		} yield (cx, cy)
		case _ 							=> Failure(new DonneesIncorectesException("Invalid limits")) 
	}
}

sealed abstract class Instruction2(value: Char) {
	override def toString(): String = value.toString()
}
object Instruction2 {
	def parse(input: Char): Try[Instruction2] = input match {
    case 'G' => Success(RotateLeft2)
    case 'D' => Success(RotateRight2)
    case 'A' => Success(Advance2)
    case _   => Failure(new DonneesIncorectesException("Invalid instruction"))
  }

  def parseMany(inputs: String): Try[List[Instruction2]] = {
    inputs.toList
      .map(input => parse(input))
      .foldLeft[Try[List[Instruction2]]](Success(Nil)) { (acc, input) =>
        (acc, input) match {
          case (Failure(e), _)            => Failure(e)
          case (_, Failure(e))            => Failure(e)
          case (Success(arr), Success(i)) => Success(arr :+ i)
        }
      }
  }
}
object Advance2 extends Instruction2('A')
object RotateLeft2 extends Instruction2('G')
object RotateRight2 extends Instruction2('D')

sealed abstract class Direction2(val value: String) {
	override def toString(): String = value

	def rotateToTheLeft(): Direction2 = this match {
		case North2 => West2
		case East2 => North2
		case West2 => South2
		case South2 => East2
	}

	def rotateToTheRight(): Direction2 = this match {
		case North2 => East2
		case East2 => South2
		case West2 => North2
		case South2 => West2
	}
}
object North2 extends Direction2("N")
object East2 extends Direction2("E")
object West2 extends Direction2("W")
object South2 extends Direction2("S")

final case class Coordinates(x: Int, y: Int, direction: Direction2) {
	private def moveForward(edges: Tuple2[Int, Int]): Coordinates = this match {
		case Coordinates(x, y, North2) if (y + 1 <= edges._2) => Coordinates(x, y + 1, North2)
		case Coordinates(x, y, West2) if (0 <= x - 1) 				=> Coordinates(x - 1, y, West2)
		case Coordinates(x, y, East2) if (x + 1 <= edges._1) 	=> Coordinates(x + 1, y, East2)
		case Coordinates(x, y, South2) if (0 <= y - 1) 				=> Coordinates(x, y - 1, South2)
		case _ 																								=> this
	}

	def followInstruction(instruction: Instruction2, edges: Tuple2[Int, Int]): Coordinates = (this, instruction) match {
		case (Coordinates(x, y, d), RotateLeft2) 	=> Coordinates(x, y, d.rotateToTheLeft())
		case (Coordinates(x, y, d), RotateRight2) => Coordinates(x, y, d.rotateToTheRight())
		case (_, Advance2) 												=> this.moveForward(edges)
	}
}

object Coordinates {
	private def parseInt(v: String, limit: Int): Try[Int] = v.toIntOption
		.flatMap(x => if (0 <= x && x <= limit) Some(x) else None)
		.fold[Try[Int]](
			Failure(new DonneesIncorectesException("Invalid coordinate value"))
		)(Success.apply)

	private def parseDirection(v: String): Try[Direction2] = v match {
		case "N" => Success(North2)
		case "E" => Success(East2)
		case "W" => Success(West2)
		case "S" => Success(South2)
		case _ 	 => Failure(new DonneesIncorectesException("Invalid direction"))
	}

	def parse(v: String, edges: Tuple2[Int, Int]): Try[Coordinates] = v.split(" ").toList match {
		case x::y::d::Nil => for {
			cx <- parseInt(x, edges._1)
			cy <- parseInt(y, edges._2)
			cd <- parseDirection(d)
		} yield Coordinates(cx, cy, cd)
		case _ 											=> Failure(new DonneesIncorectesException("Invalid coordinates")) 
	}
}

final case class Mower(start: Coordinates, instructions: List[Instruction2], worldEdges: Tuple2[Int, Int]) {
	def run(): Coordinates = instructions
		.foldLeft(start){(dest, instruction) => dest.followInstruction(instruction, worldEdges)}
}

object Mower {
	def parse(coordinates: String, instructions: String, edges: Tuple2[Int, Int]): Try[Mower] = for {
		c <- Coordinates.parse(coordinates, edges)
		i <- Instruction2.parseMany(instructions)
	} yield Mower(c, i, edges)

	def parseMany(inputs: List[String], edges: Tuple2[Int, Int]): Try[List[Mower]] = inputs match {
    case _::Nil => Failure(new DonneesIncorectesException("Invalid mowers input"))
    case coordinates::instructions::next => for {
			mower <- parse(coordinates, instructions, edges)
			rest <- parseMany(next, edges)
    } yield mower :: rest
    case Nil => Success(Nil)
  }
}