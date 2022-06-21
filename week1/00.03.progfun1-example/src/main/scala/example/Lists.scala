package example

import java.{util => ju}

object Lists:

  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def sum(xs: List[Int]): Int = if xs.isEmpty then 0 else xs.head + sum( xs.tail )

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
  def max(xs: List[Int]): Int = {

    xs.isEmpty match {
      case true => throw java.util.NoSuchElementException("Lists.max() CANNNOT be empty!!!!!")
      case false => 0
      //case selse =>
    }

    xs.tail match {
      case Nil => xs.head
      case other => {
        xs.tail.tail match {
          case Nil => if xs.head < xs.tail.head then xs.tail.head else xs.head
          case other => max ( List(xs.head, max(xs.tail) ) )

        }
      }

    }

//    if xs.isEmpty then throw java.util.NoSuchElementException("Lists.max() CANNOT be empty.") else if xs.tail.tail.isEmpty then 
//    if xs.head <  ( try { xs.tail.head } catch { case e: java.util.NoSuchElementException => xs.head - 1 } ) then xs.tail.head else xs.head 
//      else
//        max ( List( xs.head, max(xs.tail) ) )
//      //1
//      //
        
        //xs.head < xs.tail.head then xs.tail.head else max(xs)

// this was the culprit
//2

  }


