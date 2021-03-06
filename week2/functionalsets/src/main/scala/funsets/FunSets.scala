package funsets

/**
 * 2. Purely Functional Sets.
 */
trait FunSets extends FunSetsInterface:
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  override type FunSet = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: FunSet, elem: Int): Boolean = s(elem)

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): FunSet = 
    (thenumber: Int) => thenumber.equals(elem)



  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: FunSet, t: FunSet): FunSet = 
    (thenumber: Int) => s(thenumber) || t(thenumber)

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: FunSet, t: FunSet): FunSet = 
    (thenumber: Int) => s(thenumber) && t(thenumber)

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: FunSet, t: FunSet): FunSet = 
    (thenumber: Int) => 
      s(thenumber) && !t(thenumber)

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: FunSet, p: Int => Boolean): FunSet = 
    (thenumber: Int) =>
      s(thenumber) && p(thenumber)
      


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: FunSet, p: Int => Boolean): Boolean =
    var count = 0
    def iter(a: Int): Boolean =
      if a == 1000 then
        //println(a)
        //println("exiting...!")
        if contains(s, a) then p(a) 
        else if count == bound*2 then false
        else true
        end if
      else if ! contains(s, a) then
        count = count + 1
        iter(a+1)
      else
        p(a) && iter(a+1)
    iter(-bound)

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: FunSet, p: Int => Boolean): Boolean = 
    //println(printSet(filter(s,p)))
    forall(filter(s, p), p)


  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: FunSet, f: Int => Int): FunSet = 
    (thenumber: Int) => 
      exists(s, (p: Int) => f(p)==thenumber )

  /**
   * Displays the contents of a set
   */
  def toString(s: FunSet): String =
    val xs = for i <- (-bound to bound) if contains(s, i) yield i
    xs.mkString("{", ",", "}")

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: FunSet): Unit =
    println(toString(s))

object FunSets extends FunSets
