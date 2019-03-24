package gitish

import collection.immutable.ListMap

sealed trait GitObject {
  def serialize: String
}

class GitCommit(attributes: ListMap[String, List[String]], message: String) extends GitObject {
  override def serialize: String = {
    attributes.foldLeft("") {
      case (acc, (key, values)) => {
        acc + values.foldLeft("") {
          case (acc, value) => key + " " + value.replaceAll("\n", "\n ")
        } + "\n"
      }
    } + "\n" + message
  }

  def debugPrint: Unit = {
    for ((k, v) <- attributes) {
      for (i <- v) {
        println(s"KEY: $k | VALUE: $i")
      }
    }

    println(s"Message: $message")
  }
}

object GitCommit {
  def deserialize(data: String): GitCommit = {
    val l: ListMap[String, List[String]] = ListMap()

    def indexValueEnd(data: String, index: Int): Int = {
      val value: String = data.substring(index)
      val newlineIndex: Int = value.indexOf("\n")

      if (newlineIndex == -1) {
        index + data.substring(index).length - 1
      } else if (value.length - 1 == newlineIndex || value.charAt(newlineIndex + 1) != ' ') {
        index + newlineIndex
      } else {
        indexValueEnd(data, index + newlineIndex + 1)
      }
    }

    def parse(data: String, index: Int, acc: ListMap[String, List[String]]): (ListMap[String, List[String]], String) = {
      val space: Int = data.indexOf(" ")
      val newline: Int = data.indexOf("\n")

      // No spaces or a newline occurring before a space means the rest of the data is a message
      if (space < 0 || newline < space) {
        val message: String = data.substring(index)

        (acc, message)
      } else {
        val key: String = data.substring(index, space).trim
        val valueEnd: Int = indexValueEnd(data, space + 1)
        val value: String = data.substring(space + 1, valueEnd + 1).replaceAll("\n ", "\n").trim
        val newKvs = if (acc.contains(key)) {
          val prevValue: List[String] = acc(key)

          (acc - key) + (key -> (value :: prevValue))
        } else {
          acc + (key -> List(value))
        }

        parse(data.substring(valueEnd + 1), 0, newKvs)
      }
    }

    val (attributes, message): (ListMap[String, List[String]], String) =
      parse(data, 0, ListMap[String, List[String]]())

    new GitCommit(attributes, message)
  }
}
