import Set._


object BPLL {
  abstract class Imp

  case class Bln(bln : Boolean) extends Imp
  case class Cnd(tst : Imp, csq : Imp, alt : Imp) extends Imp
  case class Nml(int : Int) extends Imp
  case class Opr(nom : Symbol, opds : List[Imp]) extends Imp
  case class Var(nom : Symbol) extends Imp
  case class Wth(nom : Symbol, dfn : Imp, bod : Imp) extends Imp
  case class LtP(bnds : List[(Symbol, Imp)], bod : Imp) extends Imp 
  case class LtS(bnds : List[(Symbol, Imp)], bod : Imp) extends Imp 

  def normalize(imp : Imp) : Imp = imp match {
    case Var(nom) => sys.error("Unbound variable: " + nom.name)
    case Cnd(tst, csq, alt) => normalize(tst) match {
      case Bln(true) => normalize(csq)
      case Bln(false) => normalize(alt)
    }
    case Opr(nom, opds) => primitives(nom)(opds)
    case Wth(nom, dfn, bod) =>
      normalize(substitute(normalize(dfn), nom, bod))
    // desugaring
    case LtP(bnds, bod) => {
      def build(bnds : List[(Symbol, Imp)], noms : Set[Symbol]) : Imp =
        bnds match {
          case Nil => bod
          case (nom, dfn) :: bnds => {
            if (noms.contains(nom))
              sys.error("Duplicated variable: " + nom.name)
            else
              Wth(nom, normalize(dfn), build(bnds, noms + nom))
          }
        }
      normalize(build(bnds, empty))
    }
    // desugaring
    case LtS(bnds, bod) => {
      def fun : ((Symbol, Imp), Imp) => Imp = {
        case ((nom, dfn), bod) => Wth(nom, dfn, bod)
      }
      normalize(bnds.foldRight(bod)(fun))
    }
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
    case LtP(bnds, bod) => {
      def subst(bnds : List[(Symbol, Imp)]) : (List[(Symbol, Imp)], Boolean) =
        bnds match {
          case Nil => (Nil, true)
          case (bnm, dfn) :: bnds
            => subst(bnds) match {
                 case (bnds, free)
                   => ((bnm, substitute(sub, nom, dfn)) :: bnds,
                       free && bnm != nom)
               }
        }
      subst(bnds) match {
        case (bnds, free) =>
          LtP(bnds, if (free) substitute(sub, nom, bod) else bod)
      }
    }
    case LtS(bnds, bod) => {
      def subst(bnds : List[(Symbol, Imp)]) : (List[(Symbol, Imp)], Boolean) =
        bnds match {
          case Nil => (Nil, true)
          case (bnm, dfn) :: bnds => {
            if (bnm == nom)
              ((bnm, substitute(sub, nom, dfn)) :: bnds, false)
            else
              subst(bnds) match {
                case (bnds, free) =>
                  ((bnm, substitute(sub, nom, dfn)) :: bnds, free)
              }
          }
        }
      subst(bnds) match {
        case (bnds, free) =>
          LtS(bnds, if (free) substitute(sub, nom, bod) else bod)
      }
    }
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


import BPLL._


val imp1 : Imp =
  Wth('x, Nml(1),
      LtP(List(('x, Opr('+, List(Var('x), Nml(1)))),
               ('y, Opr('+, List(Var('x), Nml(2))))),
          Opr('+, List(Var('x), Var('y)))))

val imp2 : Imp =
  Wth('x, Nml(1),
      LtS(List(('x, Opr('+, List(Var('x), Nml(1)))),
               ('y, Opr('+, List(Var('x), Nml(2))))),
          Opr('+, List(Var('x), Var('y)))))

assert(normalize(imp1) == Nml(5))

assert(normalize(imp2) == Nml(6))

