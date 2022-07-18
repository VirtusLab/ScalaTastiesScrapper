//> using scala "3.2.1-RC1-bin-20220710-794e7c9-NIGHTLY"
//> using lib "org.scala-lang:scala3-tasty-inspector_3:3.2.1-RC1-bin-20220710-794e7c9-NIGHTLY"
//> using lib "org.apache.bcel:bcel:6.5.0"

import scala.quoted.*
import scala.tasty.inspector.*
import java.io.PrintWriter
import java.io.File
import java.io.FileOutputStream
import plugin.Cleaner
import scala.quoted.runtime.impl.printers.*
import org.apache.bcel.Repository

class MyInspector extends Inspector:
    def inspect(using Quotes)(tastys: List[Tasty[quotes.type]]): Unit =
        import quotes.reflect.*
        class Traverser extends TreeAccumulator[List[DefDef]]:
            def foldTree(defdefs: List[DefDef], tree: Tree)(owner: Symbol): List[DefDef] =
                val defdef = tree match 
                    case d: DefDef => 
                        List(d)
                    case tree =>
                        Nil
                foldOverTree(defdefs ++ defdef, tree)(owner)
        end Traverser

        tastys.flatMap { tasty =>
            val tree = tasty.ast
            (new Traverser).foldTree(List.empty, tree)(tree.symbol)
        }
        .filter(x => x.symbol.docstring.nonEmpty)
        .map { defdef =>
            Cleaner.clean(defdef.symbol.docstring.get).mkString(" ") match
                case comment if !comment.isBlank =>
                    print(defdef.toString.replaceAll("\\p{C}|\\s+|\\r$|\\\\t|\\\\n|\\\\r", ""))
                    print(";")
                    val reader = scala.util.Try(Repository.lookupClass(defdef.symbol.owner.fullName.replaceAll("\\$\\.", "\\$")).getMethods())
                    reader.toOption.fold("NO_BYTECODE")(_.toList.find(x => x.getName == defdef.symbol.name).map(
                        x => Option(x.getCode).fold("NO_BYTECODE")(_.toString.replaceAll("\n", ""))
                    ).foreach(print))
                    print(";")
                    val sourceCode = scala.util.Try(SourceCode.showTree(defdef)(SyntaxHighlight.plain, true).replaceAll("\n", ""))
                    print(sourceCode.toOption.getOrElse("NO_CODE"))
                    print(";")
                    print(comment)
                    print("\n")
                case _ =>
        }
            

@main
def inspector(args: String*): Unit =
    TastyInspector.inspectAllTastyFiles(Nil, List(args.head), args.toList.tail)(new MyInspector)
