
class Rational(x:Int,y:Int) {
  // check that we are not initialising something with 0 denominator
  require( y!=0,"denominator must be nonzero")
  // alternate constructor (for integers)
  def this(x:Int) = this(x,1)

  private def gcd(a:Int, b:Int) : Int = if (b==0) a else gcd(b,a %b)
  private val g = gcd(x,y)

  def numer = x/g
  def denom = y/g
  def multiply(z:Rational) = new Rational(numer*z.numer,denom*z.denom)
  def add(z:Rational) = new Rational(numer*z.denom + z.numer*denom, denom*z.denom)
  def subtract(z:Rational) = new Rational(numer*z.denom - z.numer*denom, denom*z.denom)
  def divide(z:Rational) = new Rational(numer*z.denom,denom*z.numer)
  //override def toString = s"$numer / $denom"
  override def toString = s"${numer} / ${denom}"

  def neg = new Rational(-numer,denom)
  def sub(z:Rational) = add(z.neg)

//  define operators
  def - (that:Rational) = this.subtract(that)
  def + (that:Rational) = this.add(that)
  def < (that:Rational) = this.less(that)
  def > (that:Rational) = that.less(this)


  def less(z:Rational)  = numer*z.denom < z.numer*denom
  def max(z:Rational) = if (z.less(this)) this else z




}

val moose = new Rational(2,3)
println(moose)

val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

// x - y - z
val s = x.sub(y).sub(z)
val s2 = x + y + z

val moose2 = new Rational(33,66)
println(moose2)