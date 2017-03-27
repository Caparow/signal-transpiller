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
  case class LabelStatement(l: Int) extends SingleStatement{val label: Int = l}
  case class GotoStatement(l: Int) extends SingleStatement{
    val str: String = "GOTO"
    val label: Int = l
  }
  case class ReturnStatement() extends SingleStatement{ val str: String = "RETURN" }
  case class AssemblyFileStatement(id: Int) extends SingleStatement{ val assemblyFileIdent: Int = id }

  class SyntaxAnalyzer(tokens: List[Token]) {
    private var i = 0
    val tree: Program = program()
    private var currentToken: Token = Token(0,0,0)

    def getToken: Token = {
      if(i-1 < tokens.length) {
        this.i += 1
        currentToken = tokens(i-1)
        currentToken
      } else {
        println("GETTING TOKEN ERROR")
        Token(0,0,0)
      }
    }

    def ERR(token: Token, msg: String): Unit = {
      println("Error found in <" + token.row
        + ", " + token.column + ">" + " token: " + token.key + ":\n\t" + msg)
    }

    def program(): Program ={
      var paramList = ListBuffer[Int]()
      var ident: Int = 0
      getToken
      if (currentToken.key == 401){
        getToken
        if (identifierTable.getOrElse(currentToken.key, 0 ) != 0){
          ident = currentToken.key
          if(getToken.key == 4){
            getToken
            while (currentToken. key != 5 && currentToken.key != 0){
              if (currentToken.key == 2)
                getToken
              else{
                paramList += currentToken.key
                getToken
              }
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

    def block: Block = {
      var declarations = ListBuffer[Int]()
      var statements = ListBuffer[SingleStatement]()
      getToken
      lazy val bl = Block(declarations.toList,statements.toList)
      //read declarations if exists
      if (currentToken.key == 404){
        getToken
        while (currentToken.key != 0) {
          if (constantsTable.getOrElse(currentToken.key, 0) != 0 || currentToken.key == 2) {
            if (currentToken.key == 2)
              getToken
            else {
              declarations += currentToken.key
              getToken
            }
          } else ERR(currentToken, "Constant expected")
        }
        getToken
      }
      //if BEGIN found
      if (currentToken.key == 402){
        // while not found END
        while(currentToken.key != 403)
          statements ++= statement
        if (currentToken.key == 403)
          bl
        else ERR(currentToken, "'END' expected")
      } else ERR(currentToken, "'BEGIN' expected")
      bl
    }

    def statement: List[SingleStatement] ={
      var stat = ListBuffer[SingleStatement]()
      getToken
      //if GOTO found
      if (currentToken.key == 405){
        stat += gotoStat
        //If RETURN found
      } else if (currentToken.key == 406){
        stat += returnStat
        //If Assembly File Ident found
      } else if (currentToken.key == 6){
        stat += assStat
        //if label found
      } else if (constantsTable.getOrElse(currentToken.key, 0) != 0){
        stat += LabelStatement(currentToken.key)
        getToken
        if(currentToken.key == 1)
          stat ++= statement
        else ERR(currentToken, "':' expected")
        if (currentToken.key != 0)
          ERR(currentToken, "';' found")
        //if single ; found
      } else if (currentToken.key == 0)
        return stat.toList
      else if(currentToken.key == 403)
        return stat.toList
      else ERR(currentToken, "Unresolved token found")
      stat.toList
    }

    def printTree(): Unit ={
      def printId(i: Int){print(i + " ")}
      def caseStatement(st: SingleStatement): String = st match{
          case GotoStatement(l) => "GOTO statement: " + l
          case ReturnStatement() => "RETURN statement"
          case AssemblyFileStatement(l) => "ASSEMBLY FILE: " + l
          case LabelStatement(l) => "LABEL: " + l
          case _ => "STATEMENT ERROR"
      }
      def printStatement(st: SingleStatement){println(caseStatement(st))}

      print("\n"+"*"*50 + "\n\tSYNTAX TREE\n" + "*"*50)
      print("\n\nProgram: " + tree.ident + " parameters: ")
      tree.parametersList.foreach(printId)
      print("\n\nLabels declarations: ")
      tree.block.declarations.foreach(printId)
      println("\n\nStatements: ")
      tree.block.statements.foreach(printStatement)
    }
  }
}



