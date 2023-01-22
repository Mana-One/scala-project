package fr.esgi.al.progfun.io

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
case class JsonObject(indent: String, content: Map[String, MyJson])
    extends MyJson {
  def toJson(): String = {
    content
      .map {
        case (key, value) =>
          s"""${indent + "\t"}"${key}": ${value.toJson()}"""
      }
      .mkString("{\n", ",\n", s"\n${indent}}")
  }
}
