object BPWL {
  abstract class Imp

  case class Bln(bln : Boolean) extends Imp
  case class Cnd(tst : Imp, csq : Imp, alt : Imp) extends Imp
  case class Nml(int : Int) extends Imp
  case class Opr(nom : Symbol, opds : List[Imp]) extends Imp
  case class Var(nom : Symbol) extends Imp
  case class Wth(nom : Symbol, dfn : Imp, bod : Imp) extends Imp

  type Env = Map[Symbol, Imp]

  def normalize(imp : Imp, env : Env) : Imp = imp match {
    case Var(nom) => env(nom)
    case Cnd(tst, csq, alt) => normalize(tst, env) match {
      case Bln(true) => normalize(csq, env)
      case Bln(false) => normalize(alt, env)
    }
    case Opr(nom, opds) => primitives(nom)(opds, env)
    case Wth(nom, dfn, bod) =>
      normalize(bod, env ++ List((nom, normalize(dfn, env))))
    case _ => imp
  }

  val primitives : Map[Symbol, (List[Imp], Env) => Imp] =
    Map('~ -> apply1AryBB(!_),
        '& -> apply2AryBBB(_&&_), '| -> apply2AryBBB(_||_),
        '+ -> apply2AryIII(_+_), '- -> apply2AryIII(_-_),
        '* -> apply2AryIII(_*_), '/ -> apply2AryIII(_/_),
        '= -> apply2AryIIB(_==_),
        '< -> apply2AryIIB(_<_), '> -> apply2AryIIB(_>_))

  def apply1AryBB(opr : Boolean => Boolean)
                 (opds : List[Imp], env : Env) : Imp = opds match {
    case imp :: Nil => normalize(imp, env) match {
      case Bln(b) => Bln(opr(b))
    }
  }

  def apply2AryBBB(opr : (Boolean, Boolean) => Boolean)
                  (opds : List[Imp], env : Env) : Imp = opds match {
    case lhs :: rhs :: Nil =>
      (normalize(lhs, env), normalize(rhs, env)) match {
        case (Bln(b1), Bln(b2)) => Bln(opr(b1, b2))
      }
  }

  def apply2AryIII(opr : (Int, Int) => Int)
                  (opds : List[Imp], env : Env) : Imp = opds match {
    case lhs :: rhs :: Nil =>
      (normalize(lhs, env), normalize(rhs, env)) match {
        case (Nml(n1), Nml(n2)) => Nml(opr(n1, n2))
      }
  }

  def apply2AryIIB(opr : (Int, Int) => Boolean)
                  (opds : List[Imp], env : Env) : Imp = opds match {
    case lhs :: rhs :: Nil =>
      (normalize(lhs, env), normalize(rhs, env)) match {
        case (Nml(n1), Nml(n2)) => Bln(opr(n1, n2))
      }
  }
}

