object Problem2 {
  abstract class Exp
  case class Var(x : Symbol) extends Exp
  case class Lam(x : Symbol, body : Exp) extends Exp
  case class App(fun : Exp, arg : Exp) extends Exp
  
  def freshName(names : Set[Symbol], default : Symbol) : Symbol = {
    var last : Int = 0
    var freshName = default  
    while (names contains freshName) {
      freshName = Symbol(default.name + last.toString)
      last += 1
    }
    freshName
  }

  def freeVars(e : Exp) : Set[Symbol] =  e match {
     case Var(x) => Set(x)
     case Lam(x, body) => freeVars(body) - x
     case App(f, a) => freeVars(f) ++ freeVars(a)
  }

  def subst(e1 : Exp, x : Symbol, e2 : Exp) : Exp = e1 match {
    case Var(y) => if (x == y) e2 else Var(y)
    case App(f, a) => App(subst(f, x, e2), subst(a, x, e2))
    case Lam(param, body) => 
      if (param == x) e1 else {
        val fvs = freeVars(body) ++ freeVars(e2)
        val newvar = freshName(fvs, param)
        Lam(newvar, subst(subst(body, param, Var(newvar)), x, e2))
      }                            
  }

  /**
   * ML-Variant:
   * fun cbn (Var x)       = Var x
   *   | cbn (Lam (x, e))  = Lam(x, nor e) 
   *   | cbn (App(e1, e2)) = 
   *     case cbn e1 of 
   *         Lam(x, e) => cbn (subst e2 (Lam(x, e)))
   *       | e1'       => App(e1', e2) end
   */
  def cbn(e : Exp) : Exp = e match {
    case Var(x)      => Var(x)
    case Lam(x, e)   => Lam(x, e)
    case App(e1, e2) => cbn(e1) match {
      case Lam(x, e) => cbn(subst(e, x, e2))
      case e3        => App(e3, e2)
    }
  }

  /**
   * ML-Variant:
   * fun nor (Var x)       = Var x
   *   | nor (Lam (x, e))  = Lam(x, nor e) 
   *   | nor (App(e1, e2)) = 
   *     case cbn e1 of 
   *         Lam(x, e) => nor (subst e2 (Lam(x, e)))
   *       | e1'       => let val e1'' = nor e1' 
   *                      in App(e1'', nor e2) end
   */
  def nor(e : Exp) : Exp = e match {
    case Var(x)      => Var(x)
    case Lam(x, e)   => Lam(x, e)
    case App(e1, e2) => cbn(e1) match {
      case Lam(x, e) => nor(subst(e, x, e2))
      case e3        => App(nor(e3), nor(e2))
    }
  }
}

