package adventofcode22

import scala.io.Source

object FileLoader {

  def getLinesFromFile(filename: String): List[String] = {
    val source = Source.fromFile(filename)
    val result = source.getLines().toList
    source.close
    result
  }

}
