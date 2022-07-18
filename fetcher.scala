//> using scala "3.2.1-RC1-bin-20220710-794e7c9-NIGHTLY"
//> using lib "org.scala-lang:scala3-tasty-inspector_3:3.2.1-RC1-bin-20220710-794e7c9-NIGHTLY"
//> using lib "org.scala-lang.modules:scala-parallel-collections_3:1.0.4"

import scala.io.Source
import scala.sys.process._
import scala.language.postfixOps
import java.io.File
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ForkJoinTaskSupport
import java.util.concurrent.ForkJoinPool
import scala.collection.parallel.CollectionConverters._

val errors: ListBuffer[String] = ListBuffer()

@main def main() =
    val task = Source.fromFile("coordinates").getLines.toList
    task.foreach { line =>
        val cp: LazyList[String] = s"cs fetch --classpath $line" lazyLines_!
        val filename = s"out/out_$line.csv"
        cp.toList match
            case Nil => 
            case head :: Nil =>
                val lib :: deps = head.split(":", 2).toList
                println(s"scala-cli --extra-jars $head inspector.scala plugin.scala -- $lib ${deps.headOption.fold("")(x => x)}" )
                s"scala-cli --extra-jars $head inspector.scala plugin.scala -- $lib ${deps.headOption.fold("")(x => x)}" #>> File(filename) !

                val lines = Source.fromFile(filename).getLines
                if !(lines.hasNext && lines.next.startsWith("DefDef")) then 
                  File(filename).delete()
                  s"echo $lib" #>> File("out/errors") !
            case _ =>
    }
