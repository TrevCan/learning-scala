abstract class IntSet:
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
end IntSet


object Empty extends IntSet:
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
  def union(set: IntSet): IntSet = set

end Empty

class NonEmpty(element: Int, leftElement: IntSet, rightElement: IntSet) extends IntSet:

  def contains(x: Int): Boolean = 
    if x < element then leftElement.contains(x)
    else if x > element then rightElement.contains(x)
    else true

  def incl(x: Int): IntSet = 
    if x < element then NonEmpty(element, leftElement.incl(x), rightElement)
    else if x > element then NonEmpty(element, leftElement, rightElement.incl(x))
    else this

  def union(set: IntSet): IntSet = 
    leftElement.union( rightElement ).union(set).incl(element)


end NonEmpty

object IntSet:
  def apply(): IntSet = Empty
  def apply(x: Int): IntSet = Empty.incl(x)
  def apply(a: Int, b: Int): IntSet = Empty.incl(a).incl(b)


@main def e() = 
  println("hello, world!")
