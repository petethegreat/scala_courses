package calculator

object Polynomial extends PolynomialInterface {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(b()*b() - 4.0*a()*c())

  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Var(
      if (delta() > 0) Set(
        (-b() + Math.sqrt(delta()))/(2.0*a()),
        (-b() - Math.sqrt(delta()))/(2.0*a())
      ) else if (delta() == 0.0) Set(-b()/2.0/a())
      else Set.empty[Double]
    )



//  def computeSolutions(a: Signal[Double], b: Signal[Double],
//                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
//    if (delta() > 0) {
//      Var(Set[Double](
//        (-b() + Math.sqrt(delta()))/(2.0*a()),
//        (-b() - Math.sqrt(delta()))/(2.0*a())))
//    }
//    else if (delta() == 0.0) { Var(Set[Double](-b()/(2.0*a())))}
//    else {Var(Set.empty[Double])}
//  }

//  def computeSolutions(a: Signal[Double], b: Signal[Double],
//        c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {Var(Set(2.0,3.5))}
}
//webUI/fastOptJS