/**
  * Created by Cypress on 24.02.2017.
  */

import Tables._
import scala.io.Source
import scala.util.control.Breaks._

package LexicalAnalyzer{

  import scala.collection.mutable.ListBuffer
  import java.io.FileNotFoundException

  case class Token(key: Int, row: Int, column: Int)

  class LexicalAnalyzer(filename: String) {
    private var analyzeRes = ListBuffer[Token]()
    private var file: String = ""
    try {
      var row = 1
      var column = 0
      var currColumn = 0
      file = Source.fromFile(filename) getLines() mkString "\n"
      var i = 0
      while (i < file.length) {
        //if first symbol of the Token is digit
        if (isDigit(file.charAt(i))){
          currColumn = i - column
          var num: String = ""
          num += file.charAt(i)
          i += 1
          while (i < file.length && isDigit(file.charAt(i))) {
            num += file.charAt(i)
            i += 1
          }
          analyzeRes += Token(addConstant(num.toInt), row, currColumn)
        } else if (isInAlphabet(file.charAt(i))){ //if the first symbol of the Token is letter
          currColumn = i - column
          var word: String = ""
          word += file.charAt(i)
          i+=1
          while (i< file.length && (isInAlphabet(file.charAt(i)) || isDigit(file.charAt(i)))){
            word += file.charAt(i)
            i += 1
          }
          if (isReservedWord(word)) {analyzeRes += Token(keyForResWord(word), row, currColumn)}
          else {analyzeRes += Token(addIdentifier(word), row, currColumn)}
        } else if ('(' == file.charAt(i)){ //if find parentheses
          i += 1
          if (i < file.length && '*' == file.charAt(i)){ //if this is comment
            i += 1
            breakable{
              while (i+1 < file.length ) {
                if ('*' != file.charAt(i) && ')' != file.charAt(i+1))
                  break
                if ('\n' == file.charAt(i))
                  row += 1
                i += 1
              }
            }
            if (i+1 >= file.length){
              println("EOF found in comment block")
              Error = true
            }

            i += 1
          } else if (i<file.length && '$' == file.charAt(i)){ //if this is assembly file declaration
            i += 1
            currColumn = i - column
            if (i<file.length && ' ' == file.charAt(i)){
              i += 1
              analyzeRes += Token(6, row, currColumn-2)
              if (isInAlphabet(file.charAt(i))){
                var ident: String = ""
                breakable{
                  while (i+2 < file.length && '$' != file.charAt(i+1) && ')' != file.charAt(i+2) && ' ' != file.charAt(i)){
                    if (isInAlphabet(file.charAt(i)) || isDigit(file.charAt(i)))
                      ident += file.charAt(i)
                    else {
                      Error = true
                      println("Assembly file identifier error: invalid ident. (" + row + ", " + currColumn + ")")
                      i = file.length
                      break()
                    }
                    i += 1
                  }
                }
                analyzeRes += Token(addIdentifier(ident), row, currColumn)
                analyzeRes += Token(7, row, i-column)
              } else {
                Error = true
                println("Assembly file identifier error: invalid ident. (" + row + ", " + currColumn + ")")
                i = file.length
              }
              i += 3
            } else {
              Error = true
              println("Assembly file declaration error: no whitespace (" + row + ", " + currColumn + ")")
              i = file.length
            }
          } else { analyzeRes += Token(4, row, i - column) } // if there is only parentheses
        } else if (isDivider(file.charAt(i))) { // if divider founded
          currColumn = i - column
          analyzeRes += Token(getDividerIndex(file.charAt(i)), row, currColumn)
          i += 1
        } else if (' ' == file.charAt(i)) { // increment *i*
          i += 1
        } else if ('\n' == file.charAt(i)) { // if EOL founded
          i += 1
          row += 1
          column = i
        } else {
          Error = true
          println("Unresolved symbol: (" + row + ", " + (i-column) + ")")
          i = file.length
        }
      }
    } catch {
      case ex: FileNotFoundException => Error = true
        println ("Missing file exception")
    }

    def getAnalyzeRes: List[Token] = analyzeRes.toList

    def printToken(t: Token): Unit = {
      println("(" + t.row.toString + " " + t.column.toString + ")\t"+ t.key.toString)
    }

    def getFile: String = file

    def printAnalyzeRes(): Unit = {
      println("Token for row/column: ")
      analyzeRes.foreach(printToken)
    }
  }
}

