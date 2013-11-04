object BPWL {
  abstract class Imp

  case class Bln(bln : Boolean) extends Imp
  case class Cnd(tst : Imp, csq : Imp, alt : Imp) extends Imp
  case class Nml(int : Int) extends Imp
  case class Opr(nom : Symbol, opds : List[Imp]) extends Imp
  case class Var(nom : Symbol) extends Imp
  case class Wth(nom : Symbol, dfn : Imp, bod : Imp) extends Imp

  def normalize(imp : Imp) : Imp = imp match {
    case Var(nom) => sys.error("Unbound variable: " + nom.name)
    case Cnd(tst, csq, alt) => normalize(tst) match {
      case Bln(true) => normalize(csq)
      case Bln(false) => normalize(alt)
    }
    case Opr(nom, opds) => primitives(nom)(opds)
    case Wth(nom, dfn, bod) =>
      normalize(substitute(normalize(dfn), nom, bod))
    case _ => imp
  }

  def substitute(sub : Imp, nom : Symbol, imp : Imp) : Imp = imp match {
    case Var(frn) => if (frn == nom) sub else imp
    case Cnd(tst, csq, alt) => Cnd(substitute(sub, nom, tst),
                                   substitute(sub, nom, csq),
                                   substitute(sub, nom, alt))
    case Opr(fnm, opds) => Opr(fnm, opds.map(substitute(sub, nom, _)))
    case Wth(bnm, dfn, bod) =>
      Wth(bnm, substitute(sub, nom, dfn),
          if (bnm == nom) bod else substitute(sub, nom, bod))
    case _ => imp
  }

  val primitives : Map[Symbol, List[Imp] => Imp] =
    Map('~ -> apply1AryBB(!_),
        '& -> apply2AryBBB(_&&_), '| -> apply2AryBBB(_||_),
        '+ -> apply2AryIII(_+_), '- -> apply2AryIII(_-_),
        '* -> apply2AryIII(_*_), '/ -> apply2AryIII(_/_),
        '= -> apply2AryIIB(_==_),
        '< -> apply2AryIIB(_<_), '> -> apply2AryIIB(_>_))

  def apply1AryBB(opr : Boolean => Boolean)
                 (opds : List[Imp]) : Imp = opds match {
    case imp :: Nil => normalize(imp) match {
      case Bln(b) => Bln(opr(b))
    }
  }

  def apply2AryBBB(opr : (Boolean, Boolean) => Boolean)
                  (opds : List[Imp]) : Imp = opds match {
    case lhs :: rhs :: Nil =>
      (normalize(lhs), normalize(rhs)) match {
        case (Bln(b1), Bln(b2)) => Bln(opr(b1, b2)) 
      }
  }

  def apply2AryIII(opr : (Int, Int) => Int)
                  (opds : List[Imp]) : Imp = opds match {
    case lhs :: rhs :: Nil =>
      (normalize(lhs), normalize(rhs)) match {
        case (Nml(n1), Nml(n2)) => Nml(opr(n1, n2))
      }
  }

  def apply2AryIIB(opr : (Int, Int) => Boolean)
                  (opds : List[Imp]) : Imp = opds match {
    case lhs :: rhs :: Nil =>
      (normalize(lhs), normalize(rhs)) match {
        case (Nml(n1), Nml(n2)) => Bln(opr(n1, n2))
      }
  }
}

