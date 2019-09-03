

trait Expr {

}

case class Number(n: Int) extends Expr {
}


case class Sum(e1: Expr, e2: Expr) extends Expr {

}

case class Prod( e1: Expr, e2: Expr) extends Expr {

}

case class Variable(x: String, defaultValue: Int) extends Expr {

}

def eval(e: Expr): Int = e match {
  case Number(n) => n
  case Sum(e1,e2) => eval(e1) + eval(e2)
  case Prod(e1,e2) => eval(e1) * eval(e2)
  case Variable(_,dv) => dv
  case _ => throw new Error("undetermined case for evaluation")
}

def show(e: Expr): String = e match {
  case Number(n) => n.toString
  case Sum(e1,e2) => show(e1) + " + " + show(e2)
  case Prod(e1,e2) => {
    {e1 match {
      case Sum(_,_) => "(" + show(e1) +")"
      case other => show(other)}} + "*" +
    {e2 match {
      case Sum(_,_) => "(" + show(e2) +")"
      case other => show(other)}}
  }
  case Variable(x,_) => x
  case _ => throw new Error("undetermined case for show")
}

val moose = Sum(Variable("y",1), Sum(Prod(Number(2),Variable("x",1)), Number(3)))
val moose2 = Prod(Sum(Variable("x",1),Number(2)),Sum(Variable("y",1),Number(3)))
eval(moose)
show(moose)

eval(moose2)
show(moose2)

val check = Sum(Prod(Number(2), Variable("x",1)), Variable("y",1))
val check2 = Prod(Sum(Number(2), Variable("x",1)), Variable("y",1))
show(check)
show(check2)