/**
  * Created by Cypress on 24.02.2017.
  */

import Tables._
import LexicalAnalyzer._
import SyntaxAnalyzer._

object Main extends App{
  val an = new LexicalAnalyzer("test.txt")
  if (!Error){
    an.printAnalyzeRes()
    printTables()
    val sa = new SyntaxAnalyzer(an.getAnalyzeRes)
    if (!Error){
      sa.printTree
    }
  }
}
