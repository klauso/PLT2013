abstract class Exp

case class Var(name : Symbol) extends Exp
case class Fun(pname : Symbol, body : Exp) extends Exp
case class App(fexp : Exp, aexp : Exp) extends Exp
case class Num(num : Int) extends Exp
case class If0(cexp : Exp, texp : Exp, fexp : Exp) extends Exp

abstract class Val

type Env = Map[Symbol, Val]
type Cnt = Val => Val

case class NumV(num : Int) extends Val
case class CloV(pname : Symbol, body : Exp, env : Env) extends Val  // closure
case class PrFV(metaf : (Exp, Env, Cnt) => Val) extends Val  // primitive function
case class CntV(cont : Cnt) extends Val  // continuation

def eval(exp : Exp, env : Env, cnt : Cnt) : Val = exp match {
  case Var(x) => cnt(env(x))
  case Num(n) => cnt(NumV(n))
  case Fun(par, bod) => cnt(CloV(par, bod, env))
  case App(fexp, aexp) =>
    eval(fexp, env,
         (fun : Val) => fun match {
           case CloV(pname, body, senv) =>
             eval(aexp, env,
                  (arg : Val) => eval(body, senv + (pname -> arg), cnt))
           case PrFV(f) => f(aexp, env, cnt)
           case CntV(c) => eval(aexp, env, c)
         })
  case If0(cexp, texp, fexp) =>
    eval(cexp, env,
         (v : Val) =>
           if (v == NumV(0)) eval(texp, env, cnt) else eval(fexp, env, cnt))
}

def add(exp : Exp, env : Env, cnt : Cnt) : Val = {
  def add2(exp2 : Exp, env2 : Env, cnt2 : Cnt) : Val =
    eval(exp, env,
         (v1 : Val) => v1 match {
           case NumV(n1) =>
             eval(exp2, env2,
                  (v2 : Val) => v2 match {
                    case NumV(n2) => cnt2(NumV(n1 + n2))
                    case _ => sys.error("rhs does not evaluates to a number")
                  })
           case _ => sys.error("rhs does not evaluates to a number")
         })
  cnt(PrFV(add2))
}

// Note that there is a recurring pattern in `add` and `mul` which can be
// abstracted away.  But for clarity, I leave them as they are.

def mul(exp : Exp, env : Env, cnt : Cnt) : Val = {
  def mul2(exp2 : Exp, env2 : Env, cnt2 : Cnt) : Val =
    eval(exp, env,
         (v1 : Val) => v1 match {
           case NumV(n1) =>
             eval(exp2, env2,
                  (v2 : Val) => v2 match {
                    case NumV(n2) => cnt2(NumV(n1 * n2))
                    case _ => sys.error("rhs does not evaluates to a number")
                  })
           case _ => sys.error("rhs does not evaluates to a number")
         })
  cnt(PrFV(mul2))
}

def callcc(exp : Exp, env : Env, cnt : Cnt) : Val =
  eval(exp, env,
       (fun : Val) => fun match {
         case CloV(par, bod, senv) =>
           eval(bod, senv + (par -> CntV(cnt)), cnt)
       })

val iniEnv : Env = Map('+ -> PrFV(add), '* -> PrFV(mul), 'callcc -> PrFV(callcc))
val iniCnt : Cnt = v => v

def evalTest(exp : Exp) : Val = eval(exp, iniEnv, iniCnt)

assert(evalTest(App(Var('callcc), Fun('k, Num(2)))) == NumV(2))
assert(evalTest(App(Var('callcc), Fun('k, App(Var('k), Num(2))))) == NumV(2))
assert(evalTest(App(Var('callcc), Fun('k, App(App(Var('+), Num(1)), App(Var('k), Num(2)))))) == NumV(2))
assert(evalTest(App(App(Var('*), Num(1)), App(Var('callcc), Fun('k, App(App(Var('+), Num(2)), App(Var('k), Num(3))))))) == NumV(3))

def letcc(kname : Symbol, body : Exp) : Exp = App(Var('callcc), Fun(kname, body))

def freshName(names: Set[Symbol], base: Symbol) : Symbol = {
  var index : Int = 0
  var fresh : Symbol = base
  while (names contains fresh) {
    fresh = Symbol(base.name + index.toString)
    index += 1
  }
  fresh
}

def freeVars(exp : Exp) : Set[Symbol] =  exp match {
  case Var(name) => Set(name)
  case Fun(pname, body) => freeVars(body) - pname
  case App(fexp, aexp) => freeVars(fexp) ++ freeVars(aexp)
  case Num(_) => Set.empty
  case If0(cexp, texp, fexp) => freeVars(cexp) ++ freeVars(texp) ++ freeVars(fexp)
}

def callcc(fexp : Exp) : Exp = {
  val kname : Symbol = freshName(freeVars(fexp), 'k)
  letcc(kname, App(fexp, Var(kname)))
}

assert(evalTest(callcc(Fun('k, Num(2)))) == NumV(2))
assert(evalTest(callcc(Fun('k, App(Var('k), Num(2))))) == NumV(2))
assert(evalTest(callcc(Fun('k, App(App(Var('+), Num(1)), App(Var('k), Num(2)))))) == NumV(2))
assert(evalTest(App(App(Var('*), Num(1)), callcc(Fun('k, App(App(Var('+), Num(2)), App(Var('k), Num(3))))))) == NumV(3))
evalTest(App(App(Var('callcc), Fun('k, Fun('x, App(App(Var('+), Num(1)), App(Var('k), ik))))), Num(2)))

