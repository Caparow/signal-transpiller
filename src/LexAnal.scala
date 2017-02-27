/**
  * Created by Cypress on 24.02.2017.
  */

import Tables._
import scala.io.Source
import scala.util.control.Breaks._

package LexAnal{

  import scala.collection.mutable.ListBuffer
  import java.io.FileNotFoundException

  case class Token(key: Int, row: Int, column: Int)

  class LexicalAnalyzer(filename: String) {
    private var analyzeRes = ListBuffer[Token]()

    try {
      var row = 0
      var column = 0
      for (line <- Source.fromFile(filename).getLines()){
        var i = 0
        while (i < line.length) {
          //if first symbol of the Token is digit
          if (isDigit(line.charAt(i))){
            column = i
            var num: String = ""
            num += line.charAt(i)
            i += 1
            while (i < line.length && isDigit(line.charAt(i))) {
              num += line.charAt(i)
              i += 1
            }
            analyzeRes += Token(addConstant(num.toInt), row, column)
          } else if (isInAlphabet(line.charAt(i))){ //if the first symbol of the Token is letter
            column = i
            var word: String = ""
            word += line.charAt(i)
            i+=1
            while (i<line.length && (isInAlphabet(line.charAt(i)) || isDigit(line.charAt(i)))){
              word += line.charAt(i)
              i += 1
            }
            if (isReservedWord(word)) {analyzeRes += Token(keyForResWord(word), row, column)}
            else {analyzeRes += Token(addIdentifier(word), row, column)}
          } else if ('(' == line.charAt(i)){ //if find parentness
            i += 1
            if (i<line.length && '*' == line.charAt(i)){ //if this is comment
              i += 1
              while (i-1 < line.length && '*' != line.charAt(i) && ')' != line.charAt(i+1))
                i += 1
              i += 2
            } else if (i<line.length && '$' == line.charAt(i)){ //if this is assembly file declaration
              i += 1
              if (i<line.length && ' ' == line.charAt(i)){
                i += 1
                column = i
                var ident: String = ""
                breakable{
                  while (i-2 < line.length && '$' != line.charAt(i+1) && ')' != line.charAt(i+2) && ' ' != line.charAt(i)){
                    if (isInAlphabet(line.charAt(i)) || isDigit(line.charAt(i)))
                      ident += line.charAt(i)
                    else {
                      println("Assembly file identifier error: (" + row.toString, ", ", column.toString )
                      i = line.length
                      break()
                    }
                    i += 1
                  }
                }
                analyzeRes += Token(addIdentifier(ident), row, column)
                i += 3
              }
              else println("Error whitespace")
            }
          } else if (' ' == line.charAt(i)){
            i += 1
          } else if (';' == line.charAt(i)) {
            analyzeRes += Token(0, row, i)
            i += 1
          } else if (':' == line.charAt(i)) {
            analyzeRes += Token(1, row, i)
            i += 1
          } else if (',' == line.charAt(i)) {
            analyzeRes += Token(2, row, i)
            i += 1
          } else if ('.' == line.charAt(i)) {
            analyzeRes += Token(3, row, i)
            i += 1
          } else {
            println("Unresolved symbol: (" + row.toString+ ", " + column.toString + ")")
            i = line.length
          }
        }
        row += 1
      }
    } catch {
      case ex: FileNotFoundException => println("Missing file exception")
    }

    def getAnalyzeRes: List[Token] = analyzeRes.toList

    def printToken(t: Token): Unit = {
      println("(" + t.row.toString + " " + t.column.toString + ") "+ t.key.toString)
    }
    def printAnalyzeRes(): Unit = {
      println("Token for row/column: ")
      analyzeRes.foreach(printToken)
    }
  }
}

