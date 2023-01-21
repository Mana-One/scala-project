package fr.esgi.al.progfun.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues._

class CoordinatesParserSpec extends AnyFunSuite {
  test("parsing should return an instance of Coordinates") {
    val coordinates = CoordinatesParser.parse("1 2 N", Limit(10, 10))
    assert(coordinates.success.value.equals(Coordinates(1, 2, North)) == true)
  }

  test("parsing should fail when format is incorrect") {
    val res = CoordinatesParser.parse("R 2 N 56", Limit(10, 10))
    assert(res.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
  }

  test("parsing should fail when out of bounds") {
    val res = CoordinatesParser.parse("6 7 E", Limit(5, 5))
    assert(res.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
  }
}