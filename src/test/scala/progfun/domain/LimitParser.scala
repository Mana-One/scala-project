package fr.esgi.al.progfun.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues._

class LimitParserSpec extends AnyFunSuite {
  test("parsing should return a Limit") {
    val res = LimitParser.parse("10 5")
    assert(res.success.value.isInstanceOf[Limit] == true)
    assert(res.success.value.equals(Limit(10, 5)))
  }

  test("parsing should fail when passing negative values") {
    val res = LimitParser.parse("-10 5")
    assert(res.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
  }

  test("parsing should fail when not passing Ints") {
    val res = LimitParser.parse("10 aaa")
    assert(res.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
  }

  test("parsing should fail when not passing exactly two Ints") {
    val res = LimitParser.parse("")
    val res2 = LimitParser.parse("1 ")
    val res3 = LimitParser.parse("1 2 3")

    assert(res.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
    assert(res2.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
    assert(res3.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
  }
}