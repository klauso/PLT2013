object Problem1 {
  abstract class Exp 

  case class Num(n : Int) extends Exp
  case class Id(x : Symbol) extends Exp 
  case class Add(lhs : Exp, rhs : Exp) extends Exp
  
  case class WithFun(f : Symbol, param : Symbol, fdef : Exp, body : Exp) extends Exp
  case class App(funExp : Exp, argExp : Exp) extends Exp
  
  abstract class Value
  
  case class NumV(n : Int) extends Value
  case class ClosureV(param : Symbol, body : Exp, env : Env) extends Value
  
  type Env = Map[Symbol, Value]
  
  def eval(exp : Exp, env : Env) : Value = exp match {
    case Num(n)   => NumV(n)
    case Add(l,r) => (eval(l,env),eval(r,env)) match {
      case (NumV(ln),NumV(rn)) => NumV(ln+rn)
      case _ => error("Type error: can only add numbers")
    }
    case Id(x)    => env(x)
    
    case WithFun(f,x,fdef,body) => eval(body, env + (f -> ClosureV(x,fdef,env)))
    case App(funExp,arg) => eval(funExp,env) match {
      case ClosureV(param,body,ienv) => eval(body,ienv + (param -> eval(arg,env))) 
      case _ => error("Type error: can only applicate functions")
    }
  }
  
  /* Comment: Excellent!  Indeed `App`ication of the temporary function to
     `xdef` should be put outside `WithFun`, otherwise, the temporary function
     name may capture free variables of the same name in `xdef`.  Once the
     application is put outside `WithFun`, the name for the temporary function
     no longer matters, as long as `WithFun` disallows the definition of a
     recursive function.  This assumption is typically true because usually
     local binding constructs that establish recursive and non-recursive
     defintions are usually syntactically distinguished.  For example, in RCWAE
     we have both `With` and `Letrec`; in Scheme/Racket we have `let`, `let*`
     and `letrec`.  But this is not always the case.  For example, in Haskell,
     `let` allows recursive definitions.  Once `WithFun` allows recursion, then
     an arbitrarily-chosen name for the temporaray function may capture free
     variables of the same name in `body`, depending on how recursive binding
     is established.  It seems that your clever trick of choosing the parameter
     name `x` as also the name for the temporaray function will not have this
     problem.  I am not sure whether it may fail in an extreme case.  That
     said, I think the risk-free solution is still generating a fresh name that
     does not occur in `body`. */
  def wth(x : Symbol, xdef : Exp, body : Exp) : Exp = App(WithFun(x, x, body, Id(x)), xdef)
  
  val exp1 : Exp = WithFun('id, 'x, Id('x), wth('id, Num(2), Id('id)))
  val exp2 : Exp = WithFun('id, 'x, Id('x), WithFun('id, 'x, Num(2), App(Id('id), Num(1))))
  val exp3 : Exp = wth('y, Num(1), WithFun('f, 'x, Add(App(Id('double), Id('x)), Id('y)), wth('g, App(Id('twice), Id('f)), App(Id('g), Num(1)))))
  
  val emptyEnv : Env = Map()
  val testEnv : Env = Map(
    'double -> ClosureV('x, Add(Id('x), Id('x)), emptyEnv),
    'twice -> ClosureV('f, WithFun('f2, 'x, App(Id('f), App(Id('f), Id('x))), Id('f2)), emptyEnv))

  def testEval1(exp : Exp) : Value = eval(exp, emptyEnv)
  def testEval2(exp : Exp) : Value = eval(exp, testEnv)

  assert(testEval1(exp1) == NumV(2))
  assert(testEval1(exp2) == NumV(2))
  assert(testEval2(exp3) == NumV(7))
}

import Problem1._

/*
Comments (Yi, 2013-11-09):

Perfect!  Except that you did not write down your conclusions.
*/

