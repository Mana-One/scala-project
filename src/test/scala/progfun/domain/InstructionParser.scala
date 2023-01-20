package fr.esgi.al.progfun.domain

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.TryValues._

class InstructionParserSpec extends AnyFunSuite {
  test("parsing should return a List of Instructions") {
    val res = InstructionParser.parseMany("GAGAGAGAA")
    assert(res.success.value.isInstanceOf[List[Instruction]] == true)
    assert(res.success.value.equals(List(
      RotateLeft, 
      Advance, 
      RotateLeft, 
      Advance, 
      RotateLeft, 
      Advance, 
      RotateLeft, 
      Advance, 
      Advance
    )))
  }

  test("parsing should fail when passing an invalid instruction") {
    val res = InstructionParser.parseMany("GAGATAGAA")
    assert(res.failure.exception.isInstanceOf[DonneesIncorectesException] == true)
  }
}