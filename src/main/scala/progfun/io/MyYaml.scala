package fr.esgi.al.progfun.io


sealed trait MyYaml {
  def toYaml(offset: String, inArray: Boolean): String
}

case class YamlInt(content: Int) extends MyYaml {
	def toYaml(offset: String, inArray: Boolean): String = content.toString()
}

case class YamlString(content: String) extends MyYaml {
	def toYaml(offset: String, inArray: Boolean): String = content
}

case class YamlObject(indent: String, content: Map[String, MyYaml]) extends MyYaml {
	def toYaml(offset: String, inArray: Boolean): String = {
		content.zipWithIndex.map {
			case (kv, idx) => kv match {
				case (key, v: YamlInt) => s"${indent + offset}${key}: ${v.toYaml(offset, false)}" 
				case (key, v: YamlString) => s"${indent + offset}${key}: ${v.toYaml(offset, false)}" 
				case (key, v: YamlArray) => s"${indent + offset}${key}: \n${v.toYaml(offset, false)}" 
				case (key, v: YamlObject) if (inArray && idx == 0) => s"${key}: \n${v.toYaml(offset, false)}"
				case (key, v: YamlObject) => s"${indent + offset}${key}: \n${v.toYaml(offset, false)}"
			}
		}.mkString("\n")
	}
}

case class YamlArray(indent: String, content: List[MyYaml]) extends MyYaml {
	def toYaml(offset: String, inArray: Boolean): String =  {
		content.map(c => s"${indent + offset}- ${c.toYaml("  ", true)}").mkString("\n")
	}
}