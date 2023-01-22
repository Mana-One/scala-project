package fr.esgi.al.progfun.io

import fr.esgi.al.progfun.domain._

sealed trait Marshaller {
  def write(limit: Limit, mowers: List[Mower]): String
}

// CSV MARSHALLER
object CSVMarshaller extends Marshaller {
  private def writeInstruction(instruction: Instruction): String = {
    instruction match {
      case Advance     => "A"
      case RotateLeft  => "G"
      case RotateRight => "D"
    }
  }

  private def writeDirection(direction: Direction): String = {
    direction match {
      case East  => "E"
      case North => "N"
      case South => "S"
      case West  => "W"
    }
  }

  private def writeCoordinates(coordinates: Coordinates): String =
    s"${coordinates.x.toString()};${coordinates.y.toString()};${writeDirection(coordinates.direction)}"

  override def write(limit: Limit, mowers: List[Mower]): String = {
    mowers.zipWithIndex
      .map(m => {
        val num = m._2 + 1
        val start = m._1.start
        val instructions = m._1.instructions
        val end = m._1.run()

        s"${num.toString()};${writeCoordinates(start)};${writeCoordinates(end)};${instructions
          .map(writeInstruction) mkString ((""))}"
      })
      .mkString("\n")
  }
}

// JSON MARSHALLER
sealed trait MyJson {
  def toJson(): String
}
case class JsonInt(content: Int) extends MyJson {
  def toJson(): String = content.toString()
}
case class JsonString(content: String) extends MyJson {
  def toJson(): String = s""""$content""""
}
case class JsonArray(content: List[MyJson]) extends MyJson {
  def toJson(): String =
    content
      .map(c => c.toJson())
      .mkString("[", ",", "]")
}
case class JsonObject(indent: Int, content: Map[String, MyJson])
    extends MyJson {
  def toJson(): String = {
    val parentIndentation = List.fill(indent)('\t').mkString
    val childIndentation = List.fill(indent + 1)('\t').mkString

    content
      .map {
        case (key, value) =>
          s"""${childIndentation}"${key}": ${value.toJson()}"""
      }
      .mkString("{\n", ",\n", s"\n${parentIndentation}}")
  }
}

object JsonMarshaller extends Marshaller {
  private def directionToJson(direction: Direction): MyJson = direction match {
    case North => JsonString("N")
    case East  => JsonString("E")
    case West  => JsonString("W")
    case South => JsonString("S")
  }

  private def coordinatesToJson(indent: Int, coordinates: Coordinates): MyJson =
    JsonObject(
      indent,
      Map(
        "point" -> JsonObject(
          indent + 1,
          Map(
            "x" -> JsonInt(coordinates.x),
            "y" -> JsonInt(coordinates.y)
          )
        ),
        "direction" -> directionToJson(coordinates.direction)
      )
    )

  private def instructionToJson(instruction: Instruction): MyJson =
    instruction match {
      case Advance     => JsonString("A")
      case RotateLeft  => JsonString("G")
      case RotateRight => JsonString("D")
    }

  private def mowerToJson(indent: Int, mower: Mower): MyJson =
    JsonObject(
      indent,
      Map(
        "début"        -> coordinatesToJson(indent + 1, mower.start),
        "instructions" -> JsonArray(mower.instructions.map(instructionToJson)),
        "fin"          -> coordinatesToJson(indent + 1, mower.run())
      )
    )

  private def limitToJson(indent: Int, limit: Limit): MyJson =
    JsonObject(
      indent,
      Map(
        "x" -> JsonInt(limit.x),
        "y" -> JsonInt(limit.y)
      )
    )

  override def write(limit: Limit, mowers: List[Mower]): String =
    JsonObject(
      0,
      Map(
        "limite"    -> limitToJson(1, limit),
        "tondeuses" -> JsonArray(mowers.map(mower => mowerToJson(1, mower)))
      )
    ).toJson()
}

sealed trait MyYaml {
  def toYaml(): String
}

case class YamlString(content: String) extends MyYaml {
  def toYaml(): String = ???
}

case class YamlArray(content: List[MyYaml]) extends MyYaml {
  def toYaml(): String = ???
}

object YamlMarshaller extends Marshaller {
  override def write(limit: Limit, mowers: List[Mower]): String = ???
}
