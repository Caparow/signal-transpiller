/**
  * Created by Cypress on 27.03.2017.
  */
import Tables._
import LexicalAnalyzer._
import scala.collection.mutable.ListBuffer

package SyntaxAnalyzer{

  trait SingleStatement
  case class Program(ident: Int, paramList: List[VarType], bl: Block){
    val str: String = "PROCEDURE"
    val procedureIdent: Int = ident
    val parametersList: List[VarType] = paramList
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
  case class AssemblyFileStatement(assemblyFileIdent: Int) extends SingleStatement
  case class DefaultStatement() extends SingleStatement
  case class SemicolonStatement() extends SingleStatement
  case class VarType(vIdent: Int, vType: Int)

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
      var paramList = ListBuffer[VarType]()
      var ident: Int = 0
      var identType: Int = 0
      //procedure
      if (getToken.key == 401){
        //procedure ident
        if (identifierTable.getOrElse(getToken.key, 0 ) != 0){
          ident = currentToken.key
          //parentheses
          if(getToken.key == 4){
            while (currentToken.key != 5 && currentToken.key != 0){
              //","
              if (identifierTable.getOrElse(currentToken.key, 0) != 0) {
                ident = currentToken.key
                getToken.key match {
                  case key if key == 1 =>
                    if (identifierTable.contains(getToken.key))
                      identType = currentToken.key
                    else ERR(currentToken, "Type excepted")
                    if (getToken.key == 2){
                      if(getToken.key == 5)
                        ERR(currentToken, "Identifier expected")
                    } else if (currentToken.key != 5) ERR(currentToken, "',' expected")
                  case key if key == 2 =>
                    identType = 0
                    if(getToken.key == 5)
                      ERR(currentToken, "Identifier expected")
                  case key if key == 5 => identType = 0
                  case _ => ERR(currentToken, "',' expected")
                }
                paramList += VarType(ident, identType)
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

    def gotoStat: SingleStatement = {
      getToken.key match {
        case key if constantsTable.getOrElse(key, 0) != 0 =>
          if (getToken.key != 0)
            ERR(currentToken, "';' expected")
          GotoStatement(key)
        case _ => ERR(currentToken, "Constant expected")
          DefaultStatement()
      }
    }

    def returnStat: SingleStatement = {
      if (getToken.key != 0)
        ERR(currentToken, "';' expected")
      ReturnStatement()
    }

    def assStat: SingleStatement = {
      getToken.key match {
        case key if identifierTable.getOrElse(key, 0) != 0 =>
          if (getToken.key != 7)
            ERR(currentToken, "' $)' expected")
          if (getToken.key != 0)
            ERR(currentToken, "';' expected")
          AssemblyFileStatement(key)
        case _ => DefaultStatement()
      }
    }

    def labelStat: SingleStatement ={
      val ident = currentToken.key
      getToken.key match {
        case key if key == 1 => LabelStatement(ident, statement)
        case _ => ERR(currentToken, "':' expected")
          DefaultStatement()
      }
    }

    def declarationsBlock: List[Int] = {
      var declarations = ListBuffer[Int]()
      while (currentToken.key != 0 && currentToken.key != -1) {
        if (constantsTable.contains(currentToken.key)) {
          declarations += currentToken.key
          if (getToken.key == 2){
            if (constantsTable.getOrElse(getToken.key, 0) == 0)
              ERR(currentToken, "Constant expected")
          } else if (currentToken.key != 0) ERR(currentToken, "',' expected")
        } else {
          getToken
        }
      }
      if (currentToken.key != 0)
        ERR(currentToken, "';' expected")
      getToken
      declarations.toList
    }

    def beginEndBlock: List[SingleStatement] ={
      var statements = ListBuffer[SingleStatement]()
      var stat: SingleStatement = DefaultStatement()
      while(currentToken.key != -1 && currentToken.key != 403){
        stat = statement
        if (stat != SemicolonStatement())
          statements += stat
      }
      statements.toList
    }

    def block: Block = {
      var declarations = List[Int]()
      var statements = List[SingleStatement]()
      lazy val bl = Block(declarations,statements)
      //read declarations if exists
      if (getToken.key == 404)
        declarations = declarationsBlock
      //if BEGIN found
      if (currentToken.key == 402){
        // while not found END
       statements = beginEndBlock
        if (currentToken.key == 403)
          bl
        else ERR(currentToken, "'END' expected")
      } else ERR(currentToken, "'BEGIN' expected")
      bl
    }

    def statement: SingleStatement ={
      getToken.key match {
        case key if key == 405 => gotoStat
        case key if key == 406 => returnStat
        case key if key == 6 => assStat
        case key if constantsTable.getOrElse(key, 0) != 0 => labelStat
        case key if key == 0 || key == 403 || key == -1 => SemicolonStatement()
        case _ => ERR(currentToken, "Unresolved token found")
          DefaultStatement()
      }
    }

    def printTree(): Unit ={
      def printId(i: Int){print(findInTables(i) + " ")}
      def printVar(v: VarType){
        print(findInTables(v.vIdent))
        if (v.vType != 0)
          print(": " + findInTables(v.vType) + ", ")
        else
          print(", ")}
      def caseStatement(st: SingleStatement, level: Int): String = st match{
          case GotoStatement(l) => "|---GOTO " + constantsTable(l)
          case ReturnStatement() => "|---RETURN"
          case AssemblyFileStatement(l) => "|---ASSEMBLY FILE: *" + identifierTable(l) + "*"
          case LabelStatement(l,s) => "|---" + constantsTable(l) + ":\n"+"|---"*level + caseStatement(s, level + 1)
          case DefaultStatement() => "ERROR IN STATEMENTS: statement doesn't initialized."
          case _ => "STATEMENT ERROR"
      }
      def printStatement(st: SingleStatement){println(caseStatement(st, 1))}

      print("\n"+"*"*50 + "\n\t\tSYNTAX TREE\n" + "*"*50)
      print("\n\nPROCEDURE " + identifierTable(tree.ident) + "(")
      tree.parametersList.foreach(printVar)
      print(")\n")
      if(tree.block.declarations.nonEmpty){
        print("|---LABEL ")
        tree.block.declarations.foreach(printId)
        print("\n")
      }
      println("BEGIN")
      tree.block.statements.foreach(printStatement)
      println("END")
      println("\n"+"*"*50)
      println("*"*50 + "\n\t\tTREE STRUCTURE\n" + "*"*50 + "\n")
      println(tree)
      println("|---"+tree.block)
      tree.block.statements.foreach((x:SingleStatement) =>println("|---|---" + x))
      println("\n"+"*"*50)
    }
  }
}



