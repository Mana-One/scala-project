package fr.esgi.al.progfun.io

import org.scalatest.funsuite.AnyFunSuite
import fr.esgi.al.progfun.domain._
import better.files.File

class MarshallerSpec extends AnyFunSuite {
  val limit = Limit(5, 5)
  val mowers = List(
    Mower(
      Coordinates(1, 2, North),
      List(
        RotateLeft,
        Advance,
        RotateLeft,
        Advance,
        RotateLeft,
        Advance,
        RotateLeft,
        Advance,
        Advance
      ),
      limit
    ),
    Mower(
      Coordinates(3, 3, East),
      List(
        Advance,
        Advance,
        RotateRight,
        Advance,
        Advance,
        RotateRight,
        Advance,
        RotateRight,
        RotateRight,
        Advance
      ),
      limit
    )
  )

  test("CsvMarshaller should write a CSV string") {
    val output = CSVMarshaller.write(limit, mowers)

    assert(output == "1;1;2;N;1;3;N;GAGAGAGAA\n2;3;3;E;5;1;E;AADAADADDA")
  }

  test("JsonMarshaller should write a JSON string") {
    val output = JsonMarshaller.write(limit, mowers)
    val json =
      File("src/test/scala/progfun/io/output.spec.json").contentAsString

    assertResult(json)(output)
  }

  test("YamlMarshaller should write a YAML string") {
    val output = YamlMarshaller.write(limit, mowers)
    val yaml: String =
      File("src/test/scala/progfun/io/output.spec.yaml").contentAsString

    assert(output === yaml)
  }

}
