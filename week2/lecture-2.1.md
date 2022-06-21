# Higher Order Functions

According to the lecture, **functions** in functional
programming languages are elemental to the language itself,
and are treated accordingly - as "*first-class*" values.

Therefore, functions themselves are values that are to be used
as results and parameters.

**Higher Order Functions** are functions that either take function(s)
as arguments/parameters _or_ return functions !

That's a lot of mumbo jumbo words right there. Try to internalize these
concepts. I had to go back to the lecture to understand this as well.

Well, that's literally the first slide of the lecture. 

After that, the author shows a set of functions that will create the
summation from a minimum to a maximum interval. He shows us several functions,
using linear recursiveness with specific functions inside of each. e.g.
sumNormal, sumCubes, sumFactorials.
all of these use the same logic behind them, 
it represents a sumation of f(n) where n goes from a minimum to a maximum value.
The only thing that changes is, well,
`f(x)`. for the `sumNormal` function `f(x)` equals `f(x) = x` because
it returns, for each number, the same one. That's how a sum is computed.
Then for sumCubes, `f(x)` would be equal to `x * x * x` and so on.

This is where Higher-order functions come in.
In other words, we're going to take *functions as parameters*, and perform
the summation just like we did in the other ones.

a function parameter is written as `f: Int => Int`. 
This function takes **one** parameter (int) and outputs an int. That's it.
If I were to want more parameters in my function, I'd sorround them in
parentheses, like so:

`f: (Int, Int, Char) => Boolean`

## anonymous functions

anonymous functions are functions without names and provide
lesser syntax compared to regular functions. Parameter types
can be omitted - if inferred from compiler.

e.g. this is an anonymous function:
`(num: Int): Int => num * 2`


