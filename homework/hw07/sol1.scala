abstract class Exp

case class Var(name : Symbol) extends Exp
case class Fun(pname : Symbol, body : Exp) extends Exp
case class App(fexp : Exp, aexp : Exp) extends Exp
case class Num(num : Int) extends Exp
case class Add(lexp : Exp, rexp : Exp) extends Exp
case class If0(cexp : Exp, texp : Exp, fexp : Exp) extends Exp
case class Wth(vname : Symbol, vdef : Exp, body : Exp) extends Exp
case class Lcc(kname : Symbol, body : Exp) extends Exp  // letcc

abstract class Val

case class NumV(num : Int) extends Val

type Env = Map[Symbol, Val]

case class CloV(pname : Symbol, body : Exp, env : Env) extends Val

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
  case Add(lexp, rexp) => freeVars(lexp) ++ freeVars(rexp)
  case If0(cexp, texp, fexp) => freeVars(cexp) ++ freeVars(texp) ++ freeVars(fexp)
  case Wth(vname, vdef, body) => freeVars(vdef) ++ freeVars(body) - vname
  case Lcc(kname, body) => freeVars(body) - kname
}

def cps(exp : Exp) : Exp = exp match {
  case Var(name) => {
    val k = freshName(Set(name), 'k)
    Fun(k, App(Var(k), exp))
  }
  case Fun(pname, body) => {
    val k = freshName(freeVars(body), 'k)
    Fun(k, App(Var(k), Fun(pname, cps(body))))
  }
  case App(fexp, aexp) => {
    val fvs1 = freeVars(fexp)
    val fvs2 = freeVars(aexp)
    val k = freshName(fvs1 ++ fvs2, 'k)
    val f : Symbol = freshName(fvs2, 'f)
    val a : Symbol = freshName(Set(k, f), 'a)
    Fun(k, App(cps(fexp),
               Fun(f, App(cps(aexp),
                          Fun(a, App(App(Var(f), Var(a)), Var(k)))))))
  }
  case Num(_) => Fun('k, App(Var('k), exp))
  case Add(lexp, rexp) => {
    val fvs1 = freeVars(lexp)
    val fvs2 = freeVars(rexp)
    val k = freshName(fvs1 ++ fvs2, 'k)
    val n1 = freshName(fvs2, 'n)
    val n2 = freshName(Set(k, n1), 'n)
    Fun(k, App(cps(lexp),
               Fun(n1, App(cps(rexp),
                           Fun(n2, App(Var(k), Add(Var(n1), Var(n2))))))))
  }
  case If0(cexp, texp, fexp) => {
    val fvs1 = freeVars(cexp)
    val fvs2 = freeVars(texp)
    val fvs3 = freeVars(fexp)
    val k = freshName(fvs1 ++ fvs2 ++ fvs3, 'k)
    val c = freshName(fvs2 ++ fvs3, 'c)
    Fun(k, App(cps(cexp),
               Fun(c, If0(Var(c),
                          App(cps(texp), Var(k)),
                          App(cps(fexp), Var(k))))))
  }
  case Wth(vname, vdef, body) => {
    val fvs = freeVars(vdef) ++ freeVars(body) - vname
    val k = freshName(fvs, 'k)
    Fun(k, App(cps(vdef),
               Fun(vname, App(cps(body), Var(k)))))
  }
  case Lcc(kname, body) => {
    val fvs = freeVars(body) - kname
    val k = freshName(fvs, 'k)

    // The only difference from the wrong definition is that `ik` is replaced
    // with `Var(k)`.  See below why it has to be done like this.
    Fun(k, App(App(Fun(kname, cps(body)),
                   Fun('v, Fun('c, App(Var(k), Var('v))))),
               Var(k)))
  }
}

/* Note that `eval` does not have a case for `Lcc`.  `cps` does _not only_
 * CPS-transformation, _but also_ desugaring.
 */
def eval(exp : Exp, env : Env) : Val = exp match {
  case Var(name) => env(name)
  case Fun(pname, body) => CloV(pname, body, env)
  case App(fexp, aexp) => eval(fexp, env) match {
    case CloV(pname, body, senv) =>
      eval(body, senv + (pname -> eval(aexp, env)))
  }
  case Num(num) => NumV(num)
  case Add(lexp, rexp) =>
    (eval(lexp, env), eval(rexp, env)) match {
      case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
    }
  case If0(cexp, texp, fexp) =>
    if (eval(cexp, env) == NumV(0))
      eval(texp, env) else eval(fexp, env)
  case Wth(vname, vdef, body) =>
    eval(body, env + (vname -> eval(vdef, env)))
}

def ik : Exp = Fun('v, Var('v))

def ie : Env = Map()

def evalTest(exp : Exp) : Val = eval(App(cps(exp), ik), ie)

/*
// Orginally, I planned to support `Letcc` or `Callcc` as syntactic sugar
// _outside_ `cps`.  That is why I had and presented the following definitions.
// Note that they are _not_ syntactic sugar yet.  See Problem 3 of Homework 8
// for the reason.  

def letcc(kname : Symbol, body : Exp) : Exp =
  App(App(Fun(kname, cps(body)),
          Fun('v, Fun('c, App(ik, Var('v))))),  // could simply be `Var('v)`
      ik)

def callcc(fexp : Exp) =
  App(cps(fexp),
      Fun('f, App(App(Var('f),
                      Fun('v, Fun('c, App(ik, Var('v))))),  // could simply be `Var('v)`
                  ik)))

// The definitions passed several tests that follow, which gave me the wrong
// impression that they were correct.

assert(evalTest(letcc('k, Num(1))) == NumV(1))
assert(evalTest(letcc('k, Add(Num(1), App(Var('k), Num(2))))) == NumV(2))
assert(evalTest(Add(Num(1), letcc('k, Add(Num(1), App(Var('k), Num(2)))))) == NumV(3))
assert(evalTest(letcc('k, If0(App(Var('k), Num(2)), Num(1), Num(0)))) == NumV(2))
assert(evalTest(App(ik, letcc('k, Add(Num(1), App(Var('k), Num(2)))))) == NumV(2))

assert(evalTest(callcc(Fun('k, Num(1)))) == NumV(1))
assert(evalTest(callcc(Fun('k, Add(Num(1), App(Var('k), Num(2)))))) == NumV(2))
assert(evalTest(Add(Num(1), callcc(Fun('k, Add(Num(1), App(Var('k), Num(2))))))) == NumV(3))
assert(evalTest(callcc(Fun('k, If0(App(Var('k), Num(2)), Num(1), Num(0))))) == NumV(2))
assert(evalTest(App(ik, callcc(Fun('k, Add(Num(1), App(Var('k), Num(2))))))) == NumV(2))

// Only later, I found that the two definitions each failed the corresponding
// test below.  The reason is that if the `body` (for `letcc`) or the body of
// of the resulting closure from `fexp` (for `callcc`) is a function (in both
// tests `Fun('x, Add(Num(1), App(Var('k), ik)))`), after CPS-transformation,
// the function will expect a continuation as an extra argument.  But here it
// will be applied to only one argument, namely `Num(2)`.  We can not simply
// feed it with an arbitrary continuation, because that may completely change
// the meaning of the original program.

assert(evalTest(App(letcc('k, Fun('x, Add(Num(1), App(Var('k), ik)))), Num(2))) == NumV(2))  // failed
assert(evalTest(App(callcc(Fun('k, Fun('x, Add(Num(1), App(Var('k), ik))))), Num(2))) == NumV(2))  // failed

// I guess the problem of this desugaring outside `cps` is that it only does a
// local CPS-transformation to a program that contains `letcc` or `callcc`:
// `body` of `letcc` and `fexp` of `call/cc`.  Then when we insert the
// transformed part back into its original context, there is already a one
// argument "gap" between the two and they no longer fit together.  There is
// some work on local/selective CPS-transformation.  But it is not worth
// introducing these complexities.  What we only need to do is to perform a
// global CPS-transformation on the whole program.  But this means we have to
// put the desugaring _inside_ `cps`.  as is done for `Lcc` above.  Then all
// tests succeed.  The case for `callcc` is left as an exercise. ;)
*/
assert(evalTest(Lcc('k, Num(1))) == NumV(1))
assert(evalTest(Lcc('k, Add(Num(1), App(Var('k), Num(2))))) == NumV(2))
assert(evalTest(Add(Num(1), Lcc('k, Add(Num(1), App(Var('k), Num(2)))))) == NumV(3))
assert(evalTest(Lcc('k, If0(App(Var('k), Num(2)), Num(1), Num(0)))) == NumV(2))
assert(evalTest(App(ik, Lcc('k, Add(Num(1), App(Var('k), Num(2)))))) == NumV(2))
assert(evalTest(App(Lcc('k, Fun('x, Add(Num(1), App(Var('k), ik)))), Num(2))) == NumV(2))

