/**
  * Created by Cypress on 27.03.2017.
  */
import Tables._
import LexicalAnalyzer._
import scala.collection.mutable.ListBuffer

package SyntaxAnalyzer{

  trait SingleStatement
  case class Program(ident: Int, paramList: List[Int], bl: Block){
    val str: String = "PROCEDURE"
    val procedureIdent: Int = ident
    val parametersList: List[Int] = paramList
    val block: Block = bl
  }
  case class Block(dec: List[Int], stat: List[SingleStatement]){
    val start: String = "BEGIN"
    val end: String = "END"
    val declarations: List[Int] = dec
    val statements: List[SingleStatement] = stat
  }
  case class LabelStatement(l: Int, stat: SingleStatement) extends SingleStatement{
    val label: Int = l
    val statement: SingleStatement = stat
  }
  case class GotoStatement(l: Int) extends SingleStatement{
    val str: String = "GOTO"
    val label: Int = l
  }
  case class ReturnStatement() extends SingleStatement{ val str: String = "RETURN" }
  case class AssemblyFileStatement(id: Int) extends SingleStatement{ val assemblyFileIdent: Int = id }
  case class DefaultStatement() extends SingleStatement {}
  case class SemicolonStatement() extends SingleStatement {}

  class SyntaxAnalyzer(tokens: List[Token]) {
    private var i = 0
    val tree: Program = program()
    private var currentToken: Token = Token(0,0,0)

    def getToken: Token = {
      if(i < tokens.length) {
        this.i += 1
        currentToken = tokens(i-1)
        currentToken
      } else {
        println("GETTING TOKEN ERROR")
        currentToken = Token(-1,0,0)
        currentToken
      }
    }

    def ERR(token: Token, msg: String): Unit = {
      Error = true
      println("Error found in <" + token.row
        + ", " + token.column + ">" + " token: " + token.key + ":\n\t" + msg)
    }

    def program(): Program ={
      var paramList = ListBuffer[Int]()
      var ident: Int = 0
      getToken
      //procedure
      if (currentToken.key == 401){
        getToken
        //procedure ident
        if (identifierTable.getOrElse(currentToken.key, 0 ) != 0){
          ident = currentToken.key
          //parentheses
          if(getToken.key == 4){
            getToken
            while (currentToken.key != 5 && currentToken.key != 0){
              //","
              if (identifierTable.getOrElse(currentToken.key, 0) != 0) {
                paramList += currentToken.key
                getToken
                if (currentToken.key == 2){
                  getToken
                  if(currentToken.key == 5)
                    ERR(currentToken, "Identifier expected")
                } else if (currentToken.key != 5) ERR(currentToken, "',' expected")
              } else getToken
            }
            if (currentToken.key == 5){
              getToken
              return Program(ident, paramList.toList, block)
            } else ERR(currentToken, "')' expected")
          } else ERR(currentToken, "'(' expected")
        } else ERR(currentToken, "Identifier expected")
      }
      Program(ident, paramList.toList, Block(List.empty,List.empty))
    }

    def gotoStat: GotoStatement ={
      val tokeng = getToken
      getToken
      if (currentToken.key != 0)
        ERR(currentToken, "';' expected")
      if (constantsTable.getOrElse(tokeng.key, 0) != 0)
        return GotoStatement(tokeng.key)
      else ERR(tokeng, "Constant expected")
      GotoStatement(0)
    }

    def returnStat: ReturnStatement = {
      getToken
      if (currentToken.key != 0)
        ERR(currentToken, "';' expected")
      ReturnStatement()
    }

    def assStat: AssemblyFileStatement = {
      val tokena = getToken
      val tokenc = getToken
      getToken
      if (tokenc.key != 7)
        ERR(tokenc, "' $)' found")
      if (currentToken.key != 0)
        ERR(currentToken, "';' expected")
      if (identifierTable.getOrElse(tokena.key, 0) != 0)
        return AssemblyFileStatement(tokena.key)
      else ERR(tokena, "Identifier expected")
      AssemblyFileStatement(0)
    }

    def labelStat: LabelStatement ={
      val ident = currentToken.key
      var stat: SingleStatement = ReturnStatement()
      getToken
      if(currentToken.key == 1)
        stat = statement
      else ERR(currentToken, "':' expected")
      if (currentToken.key != 0)
        ERR(currentToken, "';' found")
      LabelStatement(ident, stat)
    }

    def block: Block = {
      var declarations = ListBuffer[Int]()
      var statements = ListBuffer[SingleStatement]()
      getToken
      lazy val bl = Block(declarations.toList,statements.toList)
      //read declarations if exists
      if (currentToken.key == 404){
        getToken
        while (currentToken.key != 0 && currentToken.key != -1) {
          if (constantsTable.getOrElse(currentToken.key, 0) != 0) {
            declarations += currentToken.key
            getToken
            if (currentToken.key == 2){
              getToken
              if (currentToken.key < 500)
                ERR(currentToken, "Constant expected")
            } else if (currentToken.key != 0) ERR(currentToken, "',' expected")
          } else {
            getToken
          }
        }
        if (currentToken.key != 0)
          ERR(currentToken, "';' expected")
        getToken
      }
      //if BEGIN found
      if (currentToken.key == 402){
        // while not found END
        var stat: SingleStatement = DefaultStatement()
        while(currentToken.key != -1 && currentToken.key != 403){
          stat = statement
          if (stat != SemicolonStatement())
            statements += stat
        }
        if (currentToken.key == 403)
          bl
        else ERR(currentToken, "'END' expected")
      } else ERR(currentToken, "'BEGIN' expected")
      bl
    }

    def statement: SingleStatement ={
      getToken
      //if GOTO found
      if (currentToken.key == 405){
        return gotoStat
        //If RETURN found
      } else if (currentToken.key == 406){
        return returnStat
        //If Assembly File Ident found
      } else if (currentToken.key == 6){
        return assStat
        //if label found
      } else if (constantsTable.getOrElse(currentToken.key, 0) != 0){
        return labelStat
        //if single ; found
      } else if (currentToken.key == 0 ||
                 currentToken.key == 403 ||
                 currentToken.key == -1)
        return SemicolonStatement()
      else ERR(currentToken, "Unresolved token found")
      DefaultStatement()
    }

    def printTree(): Unit ={
      def printId(i: Int){print(identifierTable(i) + " ")}
      def printDec(i: Int){print(constantsTable(i) + " ")}
      def caseStatement(st: SingleStatement, level: Int): String = st match{
          case GotoStatement(l) => "|---GOTO " + constantsTable(l)
          case ReturnStatement() => "|---RETURN"
          case AssemblyFileStatement(l) => "|---ASSEMBLY FILE: *" + identifierTable(l) + "*"
          case LabelStatement(l,s) => "|---" + constantsTable(l) + ":\n"+"|---"*level + caseStatement(s, level + 1)
          case DefaultStatement() => "ERROR IN STATEMENTS: statement doesn't initialized."
          case _ => "STATEMENT ERROR"
      }
      def printStatement(st: SingleStatement){println(caseStatement(st, 1))}

      print("\n"+"*"*50 + "\n\tSYNTAX TREE\n" + "*"*50)
      print("\n\nPROCEDURE " + identifierTable(tree.ident) + "(")
      tree.parametersList.foreach(printId)
      print(")\n")
      print("|---LABEL ")
      tree.block.declarations.foreach(printDec)
      println("\nBEGIN")
      tree.block.statements.foreach(printStatement)
      println("END")
    }
  }
}



