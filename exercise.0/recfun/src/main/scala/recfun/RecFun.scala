package recfun
import scala.util.control.Breaks._

object RecFun extends RecFunInterface:

  val DEBUG_T: Boolean = true

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
    def balanceReal( parths: List[Char], opened: Int): Int = 
      debug(s"CHAR IS: '${parths.head}'")
      var countOpenParentheses: Int = opened
      debug(s"OPENED IS: '$countOpenParentheses")
      if countOpenParentheses < 0 then
        countOpenParentheses
      else
        if parths.head == '(' then 
          if ! parths.tail.isEmpty then
            countOpenParentheses = balanceReal( parths.tail, countOpenParentheses + 1 )
          else countOpenParentheses = countOpenParentheses + 1
        else 
          if ! parths.tail.isEmpty then
            countOpenParentheses = balanceReal( parths.tail, countOpenParentheses - 1 )
          else countOpenParentheses = countOpenParentheses - 1

      debug(s"  I RETURN: $countOpenParentheses")
      countOpenParentheses
      
    val charsOnlyParths = chars.filter( char => char == '(' || char == ')' ).toList
    debug(s"only parentheses: ${charsOnlyParths.mkString}")

    val output: Int = balanceReal(charsOnlyParths, 0)
    debug(s" return is $output")
    output.equals(0)

  }

  /**
   * Exercise 3
   * Exercise 3: Counting Change
   * Write a recursive function that counts how many different ways you can 
   * make change for an amount, given a list of coin denominations. For 
   * example, there are 3 ways to give change for 4 if you have coins with 
   * denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.
   *
   * Do this exercise by implementing the countChange function in 
   * RecFun.scala. This function takes an amount to change, and a list of 
   * unique denominations for the coins. Its signature is as follows:
   *
   * def countChange(money: Int, coins: List[Int]): Int
   *
   * Once again, you can make use of functions isEmpty, head and tail on the 
   * list of integers coins.
   *
   * Hint: Think of the degenerate cases. How many ways can you give change 
   * for 0 dollars? How many ways can you give change for >0 dollars, if you 
   * have no coins?
   */
  def countChange(money: Int, coins: List[Int]): Int = 
    // divide recursive function into smaller problems. 
  //
  // first problem: 
  // " how many times does x exactly fit into y ? " 
  //
  // in this analogy, x is the coin, y is the total money.
  // the first problem is contained within the below if statement
  // the function ONLY works when 
  //    the coins list length is equal to exactly one (1)
  //    the first coin is greater than zero. 
  //      because negative money doesn't exist in our function
  //      and division by zero breaks it, because... well... maths!
  // 
  // otherwise we'd like to execute
  // this (recursive) function as expected.
  //
    if coins.length == 1  then { if coins.head > 0 then {

      val firstCoin = coins.head
      val quotient = money / firstCoin
      val residue = money % firstCoin
      if quotient > 0 && residue == 0 then
        1
      else 0 
    } else 0  } 
    else {
      var totalCombinations = 0
      for ( oneCoin <- coins ) {
        totalCombinations = totalCombinations + countChange(money, List(oneCoin) )
      }

      totalCombinations

    }




  // my debug function, only works when constants DEBUG_T is true,
  // else it won't do anything.
  def debug( message: String) = if DEBUG_T then println(message) else Nil


