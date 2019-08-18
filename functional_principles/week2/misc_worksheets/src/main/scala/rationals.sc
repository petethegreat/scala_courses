
class Rational(x:Int,y:Int) {
  def numer = x
  def denom = y
  def multiply(z:Rational) = new Rational(numer*z.numer,denom*z.denom)
  def add(z:Rational) = new Rational(numer*z.denom + z.numer*denom, denom*z.denom)
  def subtract(z:Rational) = new Rational(numer*z.denom - z.numer*denom, denom*z.denom)
  def divide(z:Rational) = new Rational(numer*z.denom,denom*z.numer)
  override def toString = s"$numer / $denom"

  def neg = new Rational(-numer,denom)
  def sub(z:Rational) = add(z.neg)



}

val moose = new Rational(2,3)
println(moose)

val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

// x - y - z
val s = x.sub(y).sub(z)
