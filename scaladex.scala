//> using scala "3.2.1-RC1-bin-20220710-794e7c9-NIGHTLY"
//> using lib "org.jsoup:jsoup:1.14.3"
//> using lib "org.scala-lang.modules:scala-parallel-collections_3:1.0.4"
import org.jsoup.Jsoup
import collection.JavaConverters._
import org.jsoup.parser.Parser
import scala.collection.parallel.CollectionConverters._
import java.io._

@main 
def scaladex(args: String*) = 
    val file = "coordinates"
    val writer = BufferedWriter(FileWriter(file))
    val elems = (1 to 50).par.flatMap { page =>
        Jsoup.connect(s"https://index.scala-lang.org/search?sort=stars&languages=3.x&q=*&page=$page").get().select("h4").eachText.asScala
    }.flatMap { header =>
        val page = Jsoup.connect(s"https://index.scala-lang.org/$header/artifacts/version").get()
        val version = page.select(".head-last-version").text.trim
        page.select("option").eachText.asScala.zipAll(Nil, "", (header, version))
    }.flatMap { case (name, (header, version)) =>
        try
            val text = Jsoup.connect(s"https://index.scala-lang.org/$header/artifacts/$name/$version?binary-versions=_3").get().select("#copy-maven").text
            val doc = Jsoup.parse(text, "", Parser.xmlParser())
            if doc.select("artifactId").text.endsWith("_3") then
                val cords = doc.select("groupId").text +  ":" +  doc.select("artifactId").text + ":" + doc.select("version").text
                Some(cords)
            else 
                None
        catch 
            case e: IOException =>   
                None
    }
    elems.toList.foreach { x =>
        writer.write(x)
        writer.newLine
    }
    writer.flush
    writer.close
