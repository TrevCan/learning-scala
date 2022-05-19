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
   * 
   * Exercise 1: Pascal's Triangle
The following pattern of numbers is called Pascal's triangle.

    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1
   ...

  * The numbers at the edge of the triangle are all 1, and each number inside 
  * the triangle is the sum of the two numbers above it. Write a function 
  * that computes the elements of Pascal's triangle by means of a recursive 
  * process.

  * Do this exercise by implementing the pascal function in RecFun.scala, 
  * which takes a column c and a row r, counting from 0 and returns the 
  * number at that spot in the triangle. For example, pascal(0,2)=1, 
  * pascal(1,2)=2 and pascal(1,3)=3.

  * def pascal(c: Int, r: Int): Int
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
   *
   * Exercise 2: Parentheses Balancing
   * Write a recursive function which verifies the balancing of parentheses in
   * a string, which we represent as a List[Char] not a String. For example, 
   * The function should return true for the following strings:
   *
   * (if (zero? x) max (/ 1 x))
   * I told him (that it's not (yet) done). (But he wasn't listening)
   *
   * The function should return false for the following strings:
   *
   * :-)
   * ())(
   *
   * The last example shows that it's not enough to verify that a string
   * contains the same number of opening and closing parentheses.
   * Do this exercise by implementing the balance function in RecFun.scala. 
   * Its signature is as follows:
   *
   * def balance(chars: List[Char]): Boolean
   *
   * There are three methods on List[Char] that are useful for this exercise:
   * chars.isEmpty: Boolean returns whether a list is empty
   * chars.head: Char returns the first element of the list
   * chars.tail: List[Char] returns the list without the first element
   * Hint: you can define an inner function if you need to pass extra 
   * parameters to your function.
   * Testing: You can use the toList method to convert from 
   * a String to a List[Char]: e.g. "(just an) example".toList.
   *
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
