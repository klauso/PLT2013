object Problem3 {
  abstract class Exp 

  case class Num(n : Int) extends Exp
  case class Add(lhs : Exp, rhs : Exp) extends Exp
  case class Mul(lhs : Exp, rhs : Exp) extends Exp
  case class Id(x : Symbol) extends Exp 
  
  case class With(x : Symbol, xdef : Exp, body : Exp) extends Exp
  
  case class Call(f : Symbol, argExp : Exp) extends Exp
  
  case class Fun(f : Symbol) extends Exp
  case class App(funExp : Exp, argExp : Exp) extends Exp
  
  case class WithFun(f : Symbol, param : Symbol, fdef : Exp, body : Exp) extends Exp
  
  abstract class Value
  
  case class NumV(n : Int) extends Value
  case class ClosureV(param : Symbol, body : Exp, fenv : Funs, env : Env) extends Value
  
  type Funs = Map[Symbol, ClosureV]
  type Env = Map[Symbol, Value]
  
  def evalClosure(closure : ClosureV, arg : Value) : Value = closure match {
    case ClosureV(param,body,funs,env) => eval(body, funs, env + (param -> arg))
  }
  
  def eval(exp : Exp, funs : Funs, env : Env) : Value = exp match {
    case Num(n)   => NumV(n)
    case Add(l,r) => (eval(l,funs,env), eval(r,funs,env)) match {
      case (NumV(lv),NumV(rv)) => NumV(lv+rv)
      case _                   => error("TypeError: Can only add numbers")
    }
    case Mul(l,r) => (eval(l,funs,env), eval(r,funs,env)) match {
      case (NumV(lv),NumV(rv)) => NumV(lv*rv)
      case _                   => error("TypeError: Can only multiply numbers")
    }
    case Id(x)    => {
      if(env contains x) env(x)
      else error("undefined variable " + x.name)
    }
    
    case With(x,xdef,body) => eval(body,funs,env + (x -> eval(xdef,funs,env)))
    
    case Call(f,arg) => {
      val c = 
        if(funs contains f) funs(f) 
        else error("undefined function " + f.name)
      evalClosure(c, eval(arg,funs,env)) 
    }
    
    case Fun(x) => {
      if(funs contains x) funs(x)
      else error("undefined function " + x.name)
    }
    
    case App(funExp,arg) => {
      eval(funExp, funs, env) match {
        case c : ClosureV => evalClosure(c, eval(arg,funs,env))
        case _ => error("TypeError: can only applicate functions") 
      }
    }
    
    case WithFun(f,x,fdef,body) => eval(body,funs + (f -> ClosureV(x,fdef,funs,env)), env)
  }
  
  // Variable names refer to their bindings in the value namespace.
  val exp1 : Exp = WithFun('id, 'x, Id('x), With('id, Num(2), Id('id)))

  /* Function names refer to their bindings in the function namespace.  A
   * function name can be given directly to `Call` to invoke the function bound
   * to that name.  Since the function name will be looked up in the function
   * namespace rather than the value namespace, it can not be shadowed by the
   * same variable name. 
   */
  val exp2 : Exp = WithFun('id, 'x, Id('x), With('id, Num(2), Call('id, Num(1))))

  // But a function name can be shadowed by the same function name.
  val exp3 : Exp = WithFun('id, 'x, Id('x), 
      WithFun('id, 'x, Num(2), Call('id, Num(1))))

  /* When a function name is wrapped by `Fun` to become an `Exp`, it can no
   * longer be invoked by `Call`.  Instead, it must be invoked by `FCall`. 
   */
  val exp4 : Exp = WithFun('id, 'x, Id('x), 
      With('id, Num(2), App(Fun('id), Num(1))))

  val exp5 : Exp = With('y, Num(1), 
      WithFun('f, 'x, Add(Call('double, Id('x)), Id('y)),
          With('g, Call('twice, Fun('f)), App(Id('g), Num(1)))))

  val emptyEnv : Env = Map()
  val emptyFuns : Funs = Map()
  val testFuns : Funs = Map(
    'double -> ClosureV('x, Add(Id('x), Id('x)), emptyFuns, emptyEnv),
    'twice -> ClosureV('f, WithFun('f2, 'x, App(Id('f), App(Id('f), Id('x))), Fun('f2)), emptyFuns, emptyEnv))

  def testEval1(exp : Exp) : Value = eval(exp, emptyFuns, emptyEnv)
  def testEval2(exp : Exp) : Value = eval(exp, testFuns, emptyEnv)

  assert(testEval1(exp1) == NumV(2))
  assert(testEval1(exp2) == NumV(1))
  assert(testEval1(exp3) == NumV(2))
  assert(testEval1(exp4) == NumV(1))
  assert(testEval2(exp5) == NumV(7))
}

import Problem3._

/*
Comments (Yi, 2013-11-09):

Perfect!  Except that you did not write down your conclusions.
*/

