package recfun

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

    val onlyParths: String = chars.mkString.filter( x => { x == '(' || x == ')' } )
    println("parths is ")
    println(onlyParths)

    if chars.isEmpty || chars.tail.isEmpty || onlyParths.length % 2 != 0 then false 
    else {
      if onlyParths.length == 2 then 
        onlyParths.equals("()")

//      else {
//        if onlyParths.startsWith("(") then
//          val assumption = true
//          val newTail = onlyParths.tail
//          var trueCount: Int = 0
//          while (! newTail.tail.isEmpty)
//            if ( balance(onlyParths.tail) ) then trueCount = trueCount + 1
//            newTail = newTail.tail
//          //if (trueCount
//
//        else
//          false
//      }
      else{
        val halfParCount: Int = onlyParths.filter( x=> x == '(' ).length

        printf("halfParCount is %s\n", halfParCount)

        if halfParCount.equals(onlyParths.length / 2 ) then {
          //onlyParths
          if onlyParths.startsWith("(") then
            println(s"the tail is ${onlyParths.tail}" );
            balanceMore(onlyParths.tail.toList, true);
          else
            false

        }
        else
          false
      }
    }

    def balanceMore( s: List[Char], ignoreFirstClosed: Boolean ): Boolean = {
      if ( s.head == ')') then 
        var count = 1
        var sliced = s.mkString
        sliced = sliced.slice(count, count+1)
        while ( ! sliced.isEmpty ) {
          count = count + 1
          sliced = sliced.slice(count, count+1)
          println(balance( sliced.toList ))
        } 
        true
    else false

    }

    false
  }

    def checkInner( s: List[Char] ) : Boolean = {
      true
      // in here we know that we have an equal amount
      // of open ( and closed ) parentheses.
      // and we know that our string starts with (.
      // we should now check wether we have other 
      // unproperly closed or opened parentheses.
      // that is, checking for position.
      // e.g. ())(

//      if ( s.length == 2 )
//        return s.head.equals('(') && s.tail.head.equals(')')
//      else
//        //s.mkString.apply
//

    //}

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
