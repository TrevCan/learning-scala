class Rational ( a: Int, b: Int ):

  def this( x: Int) = this(x, 1)

  require(b > 0, "denominator must be greater than 0")

  private def greatestCommonDenominator( x: Int, y: Int): Int =
    if y == 0 then x else greatestCommonDenominator( y, x % y )

  val gcd = greatestCommonDenominator( a.abs,b.abs)

  //def numerator = a / gcd 
  //def denominator = b / gcd 
  def numerator = a 
  def denominator = b 

  def add( r: Rational): Rational =
    Rational( numerator * r.denominator + r.numerator * denominator,denominator*r.denominator)

  def multiply( r: Rational): Rational =
    Rational( numerator * r.numerator, denominator * r.denominator)

  def negate: Rational = Rational( -numerator, denominator)

  def subtract( r: Rational): Rational =
    add( r.negate )

  def less( that: Rational): Boolean =
    this.numerator * that.denominator < that.numerator * this.denominator

  def more( that: Rational): Boolean =
    that.less( this )

  def min( that: Rational): Rational = 
    if this.less( that ) then this
    else that

  def max( that: Rational): Rational =
    if this.more( that ) then this
    else that

  override def toString = s"${a/gcd} / ${b/gcd}"

end Rational
