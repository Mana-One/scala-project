package fr.esgi.al.progfun.io

sealed case class JsonIndentor(count: Int) {
  def incrementBy(inc: Int): JsonIndentor = this.copy(count + inc)

  override def toString(): String = count match {
    case c if (0 < c) => List.fill(count)("\t").mkString
    case _            => ""
  }
}
object ZeroJsonIndentor extends JsonIndentor(0)

sealed trait MyJson {
  def toJson(indent: JsonIndentor): String
}

case class JsonInt(content: Int) extends MyJson {
  def toJson(indent: JsonIndentor): String =
    s"${indent.toString()}${content.toString()}"
}
case class JsonString(content: String) extends MyJson {
  def toJson(indent: JsonIndentor): String =
    s"""${indent.toString()}"$content""""
}
case class JsonArray(content: List[MyJson]) extends MyJson {
  override def toJson(indent: JsonIndentor): String =
    content
      .map {
        case c: JsonInt    => c.toJson(ZeroJsonIndentor)
        case c: JsonString => c.toJson(ZeroJsonIndentor)
        case c: JsonArray  => c.toJson(ZeroJsonIndentor)
        case c: JsonObject => c.toJson(indent.incrementBy(1))
      }
      .mkString("[", ",", "]")
}
case class JsonObject(content: Map[String, MyJson]) extends MyJson {
  def toJson(indent: JsonIndentor): String = {
    content
      .map {
        case (key, value) =>
          value match {
            case v @ (_: JsonInt | _: JsonString | _: JsonArray) =>
              s"""${indent.incrementBy(1).toString()}"${key}": ${v.toJson(
                ZeroJsonIndentor
              )}"""

            case v: JsonObject => {
              val newIndent = indent.incrementBy(1)
              s"""${newIndent.toString()}"${key}": ${v.toJson(
                newIndent
              )}"""
            }
          }
      }
      .mkString("{\n", ",\n", s"\n${indent.toString()}}")
  }
}
