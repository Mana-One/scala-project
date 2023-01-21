package fr.esgi.al.progfun.io

import fr.esgi.al.progfun.domain._

sealed trait Marshaller {
	def write(filename: String, data: (Limit, List[Mower])): String
}

object CSVOutputMarshaller extends Marshaller {
	private def writeInstruction(instruction: Instruction): String = {
		instruction match {
			case Advance => "A"
			case RotateLeft => "G"
			case RotateRight => "D"
		}
	}

	private def writeDirection(direction: Direction): String = {
		direction match {
			case East 	=> "E"
			case North 	=> "N"
			case South 	=> "S"
			case West 	=> "W"
		}
	}

	private def writeCoordinates(coordinates: Coordinates) : String =
		s"${coordinates.x.toString()};${coordinates.y.toString()};${writeDirection(coordinates.direction)}"

  override def write(filename: String, data: (Limit, List[Mower])): String = {
		data._2.zipWithIndex.map(m => {
			val num = m._2 + 1
			val start = m._1.start
			val instructions = m._1.instructions
			val end = m._1.run()

			s"${num.toString()};${writeCoordinates(start)};${writeCoordinates(end)};${instructions.map(writeInstruction)mkString((""))}"
		}).mkString("\n")
	}
}