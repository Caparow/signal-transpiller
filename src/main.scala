/**
  * Created by Cypress on 24.02.2017.
  */

import Tables._
import LexAnal._

object mainApp {
  def main(arg: Array[String]): Unit ={
    val an = new LexicalAnalyzer("test.txt")
    an.printAnalyzeRes()
    printTables()
  }

}
