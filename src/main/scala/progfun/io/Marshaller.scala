package fr.esgi.al.progfun.io

import fr.esgi.al.progfun.domain._

sealed trait Marshaller {
  def write(limit: Limit, mowers: List[Mower]): String
}

// CSV MARSHALLER
object CSVMarshaller extends Marshaller {
  private def instructionToCsv(instruction: Instruction): MyCsv = {
    instruction match {
      case Advance     => CsvString("A")
      case RotateLeft  => CsvString("G")
      case RotateRight => CsvString("D")
    }
  }

  private def directionToCsv(direction: Direction): MyCsv = {
    direction match {
      case East  => CsvString("E")
      case North => CsvString("N")
      case South => CsvString("S")
      case West  => CsvString("W")
    }
  }

  private def coordinatesToCsv(coordinates: Coordinates): List[MyCsv] =
    CsvInt(coordinates.x) :: CsvInt(coordinates.y) :: directionToCsv(
      coordinates.direction
    ) :: Nil

  private def mowerToCsv(number: Int, mower: Mower): CsvRow = CsvRow(
    number,
    coordinatesToCsv(mower.start)
      ++ coordinatesToCsv(mower.run())
      :+ CsvArray(mower.instructions.map(instructionToCsv))
  )

  override def write(limit: Limit, mowers: List[Mower]): String = {
    CsvDocument(
      mowers.zipWithIndex.map {
        case (mower, index) => mowerToCsv(index + 1, mower)
      }
    ).toCsv()
  }
}

// JSON MARSHALLER
object JsonMarshaller extends Marshaller {
  private def directionToJson(direction: Direction): MyJson = direction match {
    case North => JsonString("N")
    case East  => JsonString("E")
    case West  => JsonString("W")
    case South => JsonString("S")
  }

  private def coordinatesToJson(
      coordinates: Coordinates
  ): MyJson =
    JsonObject(
      Map(
        "point" -> JsonObject(
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

  private def mowerToJson(mower: Mower): MyJson =
    JsonObject(
      Map(
        "début"        -> coordinatesToJson(mower.start),
        "instructions" -> JsonArray(mower.instructions.map(instructionToJson)),
        "fin"          -> coordinatesToJson(mower.run())
      )
    )

  private def limitToJson(limit: Limit): MyJson =
    JsonObject(
      Map(
        "x" -> JsonInt(limit.x),
        "y" -> JsonInt(limit.y)
      )
    )

  override def write(limit: Limit, mowers: List[Mower]): String =
    JsonObject(
      Map(
        "limite"    -> limitToJson(limit),
        "tondeuses" -> JsonArray(mowers.map(mower => mowerToJson(mower)))
      )
    ).toJson(ZeroJsonIndentor)
}

// YAML MARSHALLER
object YamlMarshaller extends Marshaller {
  private def directionToYaml(direction: Direction): MyYaml = direction match {
    case North => YamlString("N")
    case East  => YamlString("E")
    case West  => YamlString("W")
    case South => YamlString("S")
  }

  private def coordinatesToYaml(
      coordinates: Coordinates
  ): MyYaml =
    YamlObject(
      Map(
        "point" -> YamlObject(
          Map(
            "x" -> YamlInt(coordinates.x),
            "y" -> YamlInt(coordinates.y)
          )
        ),
        "direction" -> directionToYaml(coordinates.direction)
      )
    )

  private def instructionToYaml(instruction: Instruction): MyYaml =
    instruction match {
      case Advance     => YamlString("A")
      case RotateLeft  => YamlString("G")
      case RotateRight => YamlString("D")
    }

  private def mowerToYaml(mower: Mower): MyYaml =
    YamlObject(
      Map(
        "debut" -> coordinatesToYaml(mower.start),
        "instructions" -> YamlArray(
          mower.instructions.map(instructionToYaml)
        ),
        "fin" -> coordinatesToYaml(mower.run())
      )
    )

  private def limitToYaml(limit: Limit): MyYaml =
    YamlObject(
      Map(
        "x" -> YamlInt(limit.x),
        "y" -> YamlInt(limit.y)
      )
    )

  override def write(limit: Limit, mowers: List[Mower]): String =
    YamlObject(
      Map(
        "limit" -> limitToYaml(limit),
        "tondeuses" -> YamlArray(
          mowers.map(mower => mowerToYaml(mower))
        )
      )
    ).toYaml(ZeroYamlIndentator, false)
}
