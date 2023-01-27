//> using scala "3.nightly"
//> using lib "org.scala-lang:scala3-tasty-inspector_3:3.3.0-RC2"
//> using lib "org.scala-lang.modules:scala-parallel-collections_3:1.0.4"
//> using lib "io.get-coursier:coursier_2.13:2.1.0-RC5"
//> using lib "org.apache.bcel:bcel:6.7.0"
//> using lib "org.scala-lang.modules:scala-parallel-collections_3:1.0.4"

import scala.util.Try
import scala.quoted.*
import scala.tasty.inspector.*
import scala.io.Source
import scala.language.postfixOps
import java.io.File
import scala.collection.parallel.CollectionConverters._
import coursier.Dependency
import coursier.Fetch
import coursier.Module
import coursier.ModuleName
import coursier.Organization
import coursier.Repositories
import java.io.BufferedWriter
import java.io.FileWriter
import org.apache.bcel.util.SyntheticRepository
import org.apache.bcel.util.ClassPath
import org.apache.bcel.classfile.Utility
import scala.quoted.runtime.impl.printers.SyntaxHighlight

@main
def fetcher() =
  val inFile = "coordinates"
  val repositories = Seq(
    Repositories.central,
    Repositories.jcenter
  )
  val lines = scala.io.Source.fromFile(inFile).getLines()

  lines.toList.par.flatMap { case s"${organization}:${module}:${version}" =>
    val classpath = Fetch()
      .withRepositories(repositories)
      .withDependencies(
        Seq(
          Dependency(
            Module(Organization(organization), ModuleName(module)),
            version
          )
        )
      )
      .run
      .map(_.toString)

    Option.when {
      !TastyInspector.inspectAllTastyFiles(
        Nil,
        List(classpath.head),
        classpath.tail.toList
      )(
        new MyInspector(
          s"out/$organization:$module.csv",
          classpath.mkString(":")
        )
      )
    }(s"$organization:$module:$version")
  }
  .foreach(println)

class MyInspector(fileOutputName: String, classpath: String) extends Inspector:
  val file = new File(fileOutputName)
  val bw = new BufferedWriter(new FileWriter(file))
  def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
    import quotes.reflect.*
    object Traverser extends TreeAccumulator[List[DefDef]]:
      def foldTree(defdefs: List[DefDef], tree: Tree)(
          owner: Symbol
      ): List[DefDef] =
        val defdef = tree match
          case d: DefDef =>
            List(d)
          case tree =>
            Nil
        foldOverTree(defdefs ++ defdef, tree)(owner)
    end Traverser

    tastys
      .flatMap { tasty =>
        val tree = tasty.ast
        Traverser.foldTree(List.empty, tree)(tree.symbol)
      }
      .filter(_.symbol.docstring.nonEmpty)
      .flatMap { defdef =>
        val comment = Cleaner.clean(defdef.symbol.docstring.get).mkString(" ")
        Option.when(!comment.isBlank && defdef.rhs != None)(
          s"${astCode(defdef)}␟${byteCode(defdef)}␟${sourceCode(defdef, true)}␟${sourceCode(defdef, false)}␟${comment}\n"
        )
      }
      .foreach(bw.write)

    bw.close()

  extension (s: String)
    def removeNewLines: String =
      s.replaceAll("\\p{C}|\\s+|\\r$|\\\\t|\\\\n|\\\\r", " ")

  def astCode(using Quotes)(defdef: quotes.reflect.DefDef): String =
    Extractors.showTree(defdef).removeNewLines

  def byteCode(using Quotes)(defdef: quotes.reflect.DefDef): String =
    val reader = Try {
      SyntheticRepository
        .getInstance(ClassPath(classpath))
        .loadClass(defdef.symbol.owner.fullName.replaceAll("\\$\\.", "\\$"))
        .getMethods()
    }
    reader.toOption
      .flatMap {
        _.toList
          .find(_.getName == defdef.symbol.name)
          .map(_.getCode)
          .filter(_ != null)
          .map(x =>
            Utility.codeToString(x.getCode, x.getConstantPool, 0, -1, true)
          )
          .map(_.toString.removeNewLines)
      }
      .getOrElse("NO_BYTECODE")

  def sourceCode(using Quotes)(
      defdef: quotes.reflect.DefDef,
      fullNames: Boolean
  ): String =
    val sourceCode = Try(
      SourceCode
        .showTree(defdef)(SyntaxHighlight.plain, fullNames)
        .removeNewLines
    )
    sourceCode.toOption.getOrElse("NO_SOURCECODE")
