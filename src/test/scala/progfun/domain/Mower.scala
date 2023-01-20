package fr.esgi.al.progfun.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues._

class LimitSpec extends AnyFunSuite {
  test("parsing should return a pair of Int") {
    val res = Limit.parse("10 5")
    assert(res.success.value.equals((10, 5)))
  }

  test("parsing should fail when passing negative values") {
    val res = Limit.parse("-10 5")
    assert(res.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
  }

  test("parsing should fail when not passing Ints") {
    val res = Limit.parse("10 aaa")
    assert(res.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
  }

  test("parsing should fail when not passing exactly two Ints") {
    val res = Limit.parse("")
    val res2 = Limit.parse("1 ")
    val res3 = Limit.parse("1 2 3")

    assert(res.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
    assert(res2.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
    assert(res3.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
  }
}

// class MowerSpec extends AnyFunSuite {
//   test("")
// }