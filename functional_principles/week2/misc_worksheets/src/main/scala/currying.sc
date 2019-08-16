
def product(f: Int=>Int) (a:Int,b:Int) :Int = {
  if (a>b) 1 else product(f)(a+1,b)*f(a)
}

def factorial(n:Int) = product((x:Int) =>x)(1,n)


factorial(6)

//def squareprod  = product((x:Int) => x*x)
//squareprod(2,3)

product((x:Int)=>x*x)(2,3)

def prodsquares: (Int,Int)=>Int = product((x:Int)=>x*x)

prodsquares(2,3)


def CumulativeFunctionOperation(operation: (Int,Int) => Int, initVal:Int)(f:Int=>Int)(a:Int,b:Int):Int = {
  if (a>b) initVal else operation(f(a),CumulativeFunctionOperation(operation,initVal)(f)(a+1,b))
}

def sum2:(Int=>Int)=>(Int,Int)=>Int = CumulativeFunctionOperation((x:Int,y:Int)=> x+y,0)

sum2(x=>x*x)(2,3)

def prod2:(Int=>Int)=>(Int,Int)=>Int = CumulativeFunctionOperation((x:Int,y:Int)=> x*y,1)

prod2(x=>x*x)(2,3)
