package fr.esgi.al.progfun.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues._

class MowerParserSpec extends AnyFunSuite {
  test("parsing should return a List of Mowers") {
    val input = List("1 2 N", "GA", "3 3 E", "AD")
    val limit = Limit(10, 10)
    val mowers = MowerParser.parseMany(input, limit)

    assert(mowers.success.value.isInstanceOf[List[Mower]] == true)
    assert(
      mowers.success.value.equals(
        List(
          Mower(Coordinates(1, 2, North), List(RotateLeft, Advance), limit),
          Mower(Coordinates(3, 3, East), List(Advance, RotateRight), limit)
        )
      ) == true
    )
  }

  test("parsing should fail when format is invalid") {
    val input = List("1 2 N", "GA", "AD")
    val limit = Limit(10, 10)
    val res = MowerParser.parseMany(input, limit)

    assert(
      res.failure.exception.isInstanceOf[DonneesIncorectesException] == true
    )
  }
}
