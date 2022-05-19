package recfun
import scala.util.control.Breaks._

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if c == 0 then 1
    else if r == 0 then if c == 0 then 1 else 0
    else {
      pascal(c-1, r-1) + pascal(c, r-1)
    }

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {

    val charsOnlyParths = chars.filter( char => char == '(' || char == ')' ).toList

    var index = 0;
    var countOpenParentheses = 0;

    breakable{
    for ( c <- charsOnlyParths )
      if c == '(' then countOpenParentheses = countOpenParentheses + 1
      else if countOpenParentheses == 0 then 
        countOpenParentheses = countOpenParentheses - 1
        break()
      else countOpenParentheses = countOpenParentheses - 1
      //println(c)
    }
    return countOpenParentheses.equals(0)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
