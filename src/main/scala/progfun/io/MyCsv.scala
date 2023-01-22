package fr.esgi.al.progfun.io

sealed trait MyCsv {
  def toCsv(): String
}
case class CsvInt(content: Int) extends MyCsv {
  def toCsv(): String = content.toString()
}
case class CsvString(content: String) extends MyCsv {
  def toCsv(): String = content
}
case class CsvArray(content: List[MyCsv]) extends MyCsv {
  def toCsv(): String = content.map(c => c.toCsv()).mkString
}
case class CsvRow(number: Int, content: List[MyCsv]) extends MyCsv {
  def toCsv(): String =
    (CsvInt(number) +: content)
      .map(c => c.toCsv())
      .mkString(";")
}
case class CsvDocument(content: List[CsvRow]) extends MyCsv {
  def toCsv(): String = content.map(c => c.toCsv()).mkString("\n")
}
