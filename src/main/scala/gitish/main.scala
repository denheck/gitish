package gitish

import java.io.File
import java.io.PrintWriter
import java.nio.file.{Paths, Path, Files}
import java.util.zip.Inflater

object Main {
  def main(args: Array[String]): Unit = {
    println(findGitDir("/Users/dennis/x/gitish"))
    println(catFile("commit", "01d53d885c247ece575812a3cea08d9503c5a2ec"))
  }

  private def execCommand(args: Array[String]): Unit = {
    if (args.length > 0) {
        args(0) match {
          case "init" => createRepo(System.getProperty("user.dir"))
          case "cat-file" => catFile(args(1), args(2))
          case _ => println("gittish usage")
        }
      } else {
        println("gittish usage")
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

  private def catFile(_type: String, objectHash: String): String = {
    val cwd = System.getProperty("user.dir")

    findGitDir(cwd) match {
      case Some(gitDir) => {
        val (prefix, remainingHash) = objectHash.splitAt(2)
        val objectFile = Paths.get(gitDir, "objects", prefix, remainingHash)
        val objectBytes: Array[Byte] = Files.readAllBytes(objectFile)
        val inflater = new Inflater

        inflater.setInput(objectBytes)

        val decompressedData = new Array[Byte](objectBytes.size * 2)
        var count = inflater.inflate(decompressedData)
        var finalData = decompressedData.take(count)
        while (count > 0) {
          count = inflater.inflate(decompressedData)
          finalData = finalData ++ decompressedData.take(count)
        }

        new String(finalData.map(_.toChar))
      }
      case None => throw new Exception("Unable to find .git in " + cwd)
    }
  }

  // search provided directory and parent directories for ".git"
  private def findGitDir(directory: String): Option[String] = {
    val p: Path = Paths.get(directory)

    if (p == p.getRoot) {
      None
    } else if (Files.isDirectory(Paths.get(directory, ".git"))) {
      Some(Paths.get(directory, ".git").toString)
    } else {
      findGitDir(Paths.get(directory).getParent.toString)
    }
  }
}
