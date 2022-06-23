// Lecture 2.2 - currying
// write a product function that calculates the product of the values
// of a function for the points on a given interval.
def product ( fx: Int => Int ) ( low: Int, high: Int ): Int =
  if low > high then
    1
  else
    fx(low) * product( fx ) ( low + 1, high )

// a factorial function that is defined
// using the 'product' function above.
def factorial ( num: Int ): Int = product( x => x )(1, num)

// intervalCombine
def intervalCombine ( outOfBounds: Int, point: Int => Int, join: (Int, Int) => Int )(x: Int, y: Int): Int =
  if x > y then
    outOfBounds
  else join( point(x), intervalCombine(outOfBounds, point, join)(x+1, y) )

// aa

// the below function definitions use intervalCombine()

def productNew ( fx: Int => Int ) = intervalCombine( 1, fx, (a,b)=> a*b )

def sum ( fx: Int => Int ) = intervalCombine(0, fx, (a,b) => a+b )

// mapReduce without recur function
// this is the same as intervalCombine
def mapReduce( f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = 
    if a > b then zero
    else combine( f(a), mapReduce(f, combine, zero)(a+1, b))

// mapReduce verbose copy from Lecture 2.2 - Currying
def mapReduceMod( f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = 
    def recur(a: Int): Int = 
        if a > b then zero
        else combine( f(a), recur(a+1))
    recur(a)
