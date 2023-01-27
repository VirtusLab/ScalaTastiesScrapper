//> using scala "3.nightly"
//> using lib "org.jsoup:jsoup:1.15.3"
//> using lib "org.scala-lang.modules:scala-parallel-collections_3:1.0.4"

import org.jsoup.Jsoup
import scala.util.Try
import collection.JavaConverters._
import org.jsoup.parser.Parser
import scala.collection.parallel.CollectionConverters._
import java.io._

@main
def scaladex() =
  val file = "coordinates"
  val writer = BufferedWriter(FileWriter(file))
  val elems = (1 to 64).par
    .flatMap { page =>
      Jsoup
        .connect(
          s"https://index.scala-lang.org/search?sort=stars&languages=3.x&q=*&page=$page"
        )
        .get()
        .select("h4")
        .eachText
        .asScala
    }
    .flatMap { header =>
      Try(
        Jsoup
          .connect(s"https://index.scala-lang.org/$header/artifacts/version")
          .get()
      ).toOption
        .map { page =>
          val version = page.select(".head-last-version").text.trim
          page.select("option").eachText.asScala.map((_, (header, version)))
        }
    }
    .flatten
    .flatMap { case (name, (header, version)) =>
      Try {
        val text = Jsoup
          .connect(
            s"https://index.scala-lang.org/$header/artifacts/$name/$version?binary-versions=_3"
          )
          .get()
          .select("#copy-maven")
          .text
        Jsoup.parse(text, "", Parser.xmlParser())
      }.toOption
        .filter(_.select("artifactId").text.endsWith("_3"))
        .map { doc =>
          doc.select("groupId").text + ":" + doc
            .select("artifactId")
            .text + ":" + doc.select("version").text + "\n"
        }
    }
  elems.toList.foreach(writer.write)
  writer.flush
  writer.close
