package fr.esgi.al.progfun.io

sealed case class YamlIndentor(count: Int) {
  def incrementBy(inc: Int): YamlIndentor = this.copy(count + inc)

  override def toString(): String = count match {
    case c if (0 < c) => List.fill(count)("  ").mkString
    case _            => ""
  }
}
object ZeroYamlIndentator extends YamlIndentor(0)

sealed trait MyYaml {
  def toYaml(indent: YamlIndentor, inArray: Boolean): String
}

case class YamlInt(content: Int) extends MyYaml {
  def toYaml(indent: YamlIndentor, inArray: Boolean): String =
    content.toString()
}

case class YamlString(content: String) extends MyYaml {
  def toYaml(indent: YamlIndentor, inArray: Boolean): String = content
}

case class YamlObject(content: Map[String, MyYaml]) extends MyYaml {
  private def handleIntChild(
      indent: YamlIndentor,
      key: String,
      child: YamlInt
  ): String =
    s"${indent.toString()}${key}: ${child.toYaml(ZeroYamlIndentator, false)}"

  private def handleStringChild(
      indent: YamlIndentor,
      key: String,
      child: YamlString
  ): String =
    s"${indent.toString()}${key}: ${child.toYaml(ZeroYamlIndentator, false)}"

  private def handleArrayChild(
      indent: YamlIndentor,
      key: String,
      child: YamlArray
  ): String =
    s"${indent.toString()}${key}: ${child.toYaml(indent.incrementBy(1), false)}"

  private def handleObjectChild(
      index: Int,
      inArray: Boolean,
      indent: YamlIndentor,
      key: String,
      child: YamlObject
  ): String = (index, inArray) match {
    case (0, true) =>
      s"${key}: \n${child.toYaml(indent.incrementBy(1), false)}"
    case (0, _) =>
      s"${indent.toString()}${key}: \n${child.toYaml(indent.incrementBy(1), false)}"
    case (_, _) =>
      s"${indent.toString()}${key}: \n${child.toYaml(indent.incrementBy(1), false)}"
  }

  def toYaml(indent: YamlIndentor, inArray: Boolean): String = {
    content.zipWithIndex
      .map {
        case (kv, idx) =>
          kv match {
            case (key, v: YamlInt)    => handleIntChild(indent, key, v)
            case (key, v: YamlString) => handleStringChild(indent, key, v)
            case (key, v: YamlArray)  => handleArrayChild(indent, key, v)
            case (key, v: YamlObject) =>
              handleObjectChild(idx, inArray, indent, key, v)
          }
      }
      .mkString("\n")
  }
}

case class YamlArray(content: List[MyYaml]) extends MyYaml {
  def toYaml(indent: YamlIndentor, inArray: Boolean): String = {
    content
      .map(
        c => s"${indent.toString()}- ${c.toYaml(indent.incrementBy(1), true)}"
      )
      .mkString("\n", "\n", "")
  }
}
