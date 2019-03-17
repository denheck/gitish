package gitish

import java.io.File
import java.io.PrintWriter
import java.nio.file.Paths

object Main {
  def main(args: Array[String]): Unit = {
    args.length match {
      case 1 => {
        args(0) match {
          case "init" => createRepo(System.getProperty("user.dir"))
          case _ => println("gittish usage")
        }
      }
      case _ => println("gittish usage")
    }
  }

  private def createRepo(path: String): Unit = {
    if (!new File(path).exists) {
      throw new Exception("Path doesn't exist: " + path)
    }
  
    Paths.get(path, ".git").toFile.mkdirs
    Paths.get(path, ".git/branches").toFile.mkdirs
    Paths.get(path, ".git/objects").toFile.mkdirs
    Paths.get(path, ".git/refs/tags").toFile.mkdirs
    Paths.get(path, ".git/refs/heads").toFile.mkdirs

    val writer = new PrintWriter(Paths.get(path, ".git/HEAD").toFile)

    writer.write("ref: refs/heads/master\n")
  }
}
