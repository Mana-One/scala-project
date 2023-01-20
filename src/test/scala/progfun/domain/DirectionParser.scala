package fr.esgi.al.progfun.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues._

class DirectionParserSpec extends AnyFunSuite {
  test("parsing should return a Direction") {
    val north = DirectionParser.parse("N")
    val east  = DirectionParser.parse("E")
    val west  = DirectionParser.parse("W")
    val south = DirectionParser.parse("S")

    assert(north.success.value.equals(North) == true)
    assert(east.success.value.equals(East) == true)
    assert(west.success.value.equals(West) == true)
    assert(south.success.value.equals(South) == true)
  }

  test("parsing should fail when passing an invalid value") {
    val res = DirectionParser.parse("F")
    assert(res.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
  }
}