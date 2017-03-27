/**
  * Created by Cypress on 24.02.2017.
  */

object Tables {

  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".toList
  val numbers = "1234567890".toList
  val dividers = ";:,.()".toList
  val reversedKeywordsTable = Map(
    401 -> "PROCEDURE",
    402 -> "BEGIN",
    403 -> "END",
    404 -> "LABEL",
    405 -> "GOTO",
    406 -> "RETURN")

  var constantsTable =  Map[Int, Int]()
  var identifierTable = Map[Int, String]()

  def isInAlphabet(ch: Char): Boolean = alphabet.contains(ch)
  def isDigit(ch: Char): Boolean = numbers.contains(ch)
  def isDivider(ch: Char): Boolean = dividers.contains(ch)
  def getDividerIndex(ch: Char): Int = dividers.indexOf(ch)
  def isReservedWord(word: String): Boolean = reversedKeywordsTable.values.exists(_ == word)
  def isConstant(num: Int): Boolean = constantsTable.values.exists(_ == num)
  def isIdentifier(ident: String): Boolean = identifierTable.values.exists(_ == ident)

  def keyForResWord(value: String): Int = {
    val revMap = reversedKeywordsTable map { case (k, v) => (v, k) }
    revMap(value)
  }

  def addConstant(num: Int): Int ={
    if (!isConstant(num)){
      if (constantsTable.isEmpty){
        constantsTable += (501 -> num)
        501
      } else{
        constantsTable += (constantsTable.keysIterator.max + 1 -> num)
        constantsTable.keysIterator.max
      }
    } else {
      val revMap = constantsTable map { case (k, v) => (v, k) }
      revMap(num)
    }
  }

  def addIdentifier(ident: String): Int ={
    if (!isIdentifier(ident)){
      if (identifierTable.isEmpty){
        identifierTable += (1001 -> ident)
        1001
      } else{
        identifierTable += (identifierTable.keysIterator.max + 1 -> ident)
        identifierTable.keysIterator.max
      }
    } else {
      val revMap = identifierTable map { case (k, v) => (v, k) }
      revMap(ident)
    }
  }

  def printTables(): Unit = {
    println("\n\tReserved words:")
    reversedKeywordsTable.foreach(print)
    println("\n\n\tConstants:")
    constantsTable.foreach(print)
    println("\n\n\tIdentifiers:")
    identifierTable.foreach(print)
    print("\n")
  }
}
