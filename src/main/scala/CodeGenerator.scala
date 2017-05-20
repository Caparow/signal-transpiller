/**
  * Created by Cypress on 20.05.2017.
  */
import Tables._
import LexicalAnalyzer._
import SyntaxAnalyzer._
import scala.io.Source

package CodeGenerator{

  import java.io.FileNotFoundException

  class CodeGenerator(tree: Program) {
    var code: String = ""
    var labelsTable = Map[Int, String]()

    def ERR(msg: String): Unit = {
      Error = true
      println("Error found:" + msg)
    }

    code += identifierTable(tree.ident) + " PROC NEAR\n"
    if (tree.parametersList.nonEmpty) {
      code += "ARG "
      tree.parametersList.foreach(parametersGen(_))
      code = code.dropRight(1)
      code+="\n"
    }
    if (tree.block.declarations.nonEmpty) {
          labelsTable = createLabelsTable(tree.block.declarations)
    }
    if (tree.block.statements.nonEmpty)
      tree.block.statements.foreach(statementsGen)
    code+="ENDP"


    def labelGen(labelStatement: LabelStatement): Unit ={
      if (labelsTable.contains(labelStatement.label)) {
        code += labelsTable(labelStatement.label) + ": "
        statementsGen(labelStatement.statement)
      }
      else ERR("Labels declaration error.")
    }

    def gotoGen(gotoStatement: GotoStatement): Unit ={
      if (labelsTable.contains(gotoStatement.label))
        code += "\tjmp " + labelsTable(gotoStatement.label) + "\n"
      else ERR("Labels declaration error.")
    }

    def returnGen(returnStatement: ReturnStatement): Unit ={
      code += "\tRET\n"
    }

    def assemblyFileGen(assemblyFileStatement: AssemblyFileStatement): Unit ={
      try {
        val file = Source.fromFile(identifierTable(assemblyFileStatement.assemblyFileIdent)) getLines() mkString "\n"
        code+=";Assembly File Added\n"+file+"\n;End Of File\n"
      } catch {
        case ex: FileNotFoundException => ERR("assembly file doesn't found.")
      }
    }

    def createLabelsTable(labels: List[Int]): Map[Int, String] = {
      var newLabels = Map[Int, String]()
      for (l <- labels)
        newLabels += (l -> labelsChange(l))
      newLabels
    }

    def labelsChange(l: Int): String = {
      "@"+findInTables(l).toString.drop(1)
    }

    def parametersGen(p: VarType): Unit = {
      code += identifierTable(p.vIdent)
      if (p.vType != 0)
        code += ":"+identifierTable(p.vType)
      code += ","
    }

    def statementsGen(st: SingleStatement): Unit ={
      st match {
        case GotoStatement(l) => gotoGen(GotoStatement(l))
        case ReturnStatement() => returnGen(ReturnStatement())
        case AssemblyFileStatement(l) => assemblyFileGen(AssemblyFileStatement(l))
        case LabelStatement(l,s) => labelGen(LabelStatement(l,s))
      }
    }
  }
}
