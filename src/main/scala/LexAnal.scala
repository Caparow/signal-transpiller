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
      val file = Source.fromFile(filename) getLines() mkString "\n"
      var i = 0
      while (i < file.length) {
        //if first symbol of the Token is digit
        if (isDigit(file.charAt(i))){
          column = i
          var num: String = ""
          num += file.charAt(i)
          i += 1
          while (i < file.length && isDigit(file.charAt(i))) {
            num += file.charAt(i)
            i += 1
          }
          analyzeRes += Token(addConstant(num.toInt), row, i-column)
        } else if (isInAlphabet(file.charAt(i))){ //if the first symbol of the Token is letter
          column = i
          var word: String = ""
          word += file.charAt(i)
          i+=1
          while (i< file.length && (isInAlphabet(file.charAt(i)) || isDigit(file.charAt(i)))){
            word += file.charAt(i)
            i += 1
          }
          if (isReservedWord(word)) {analyzeRes += Token(keyForResWord(word), row, i-column)}
          else {analyzeRes += Token(addIdentifier(word), row, i-column)}
        } else if ('(' == file.charAt(i)){ //if find parentheses
          i += 1
          if (i<file.length && '*' == file.charAt(i)){ //if this is comment
            i += 1
            while (i-1 < file.length && '*' != file.charAt(i) && ')' != file.charAt(i+1)) {
              if ('\n' == file.charAt(i))
                row += 1
              i += 1
            }
            i += 2
          } else if (i<file.length && '$' == file.charAt(i)){ //if this is assembly file declaration
            i += 1
            column = i
            if (i<file.length && ' ' == file.charAt(i)){
              i += 1
              if (isInAlphabet(file.charAt(i))){
                var ident: String = ""
                breakable{
                  while (i-2 < file.length && '$' != file.charAt(i+1) && ')' != file.charAt(i+2) && ' ' != file.charAt(i)){
                    if (isInAlphabet(file.charAt(i)) || isDigit(file.charAt(i)))
                      ident += file.charAt(i)
                    else {
                      println("Assembly file identifier error: invalid ident. (" + row + ", " + (i-column) + ")")
                      i = file.length
                      break()
                    }
                    i += 1
                  }
                }
                analyzeRes += Token(addIdentifier(ident), row, i-column)
              } else {
                println("Assembly file identifier error: invalid ident. (" + row + ", " + (i+2 - column) + ")")
                i = file.length
              }
              i += 3
            } else {
              println("Assembly file declaration error: no whitespace (" + row + ", " + (i+2 - column) + ")")
              i = file.length
            }
          } else { analyzeRes += Token(4, row, i) }
        } else if (isDivider(file.charAt(i))) {
          analyzeRes += Token(getDividerIndex(file.charAt(i)), row, i-column)
          i += 1
        } else if (' ' == file.charAt(i)){
          i += 1
        } else if ('\n' == file.charAt(i)) {
          i += 1
          row += 1
        } else {
          println("Unresolved symbol: (" + row + ", " + (i-column) + ")")
          i = file.length
        }
      }
    } catch {
      case ex: FileNotFoundException => println ("Missing file exception")
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

