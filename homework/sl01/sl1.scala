object AExp {
  abstract class Exp

  case class Num(int : Int) extends Exp
  case class Add(lhs : Exp, rhs : Exp) extends Exp
  case class Mul(lhs : Exp, rhs : Exp) extends Exp
  case class Var(sym : Symbol) extends Exp

  implicit def toNum(int : Int) : Exp = Num(int)
  implicit def toVar(sym : Symbol) : Exp = Var(sym)

  type Env = Map[Symbol, Int]

  def evaluate(exp : Exp, env : Env) : Int = exp match {
    case Num(int) => int
    case Add(lhs, rhs) => evaluate(lhs, env) + evaluate(rhs, env)
    case Mul(lhs, rhs) => evaluate(lhs, env) * evaluate(rhs, env)
    case Var(sym) => env(sym)
  }

  // In functional style, extending the language with new operations (methods)
  // is easy.
  def print(exp : Exp) = pretty(0, exp)

  private def pretty(sup : Int, exp : Exp) : String = {
    def group(sub : Int, lhs : Exp, bop : Symbol, rhs : Exp) = {
      val ppe = pretty(sub, lhs) + " " + bop.name + " " + pretty(sub, rhs)
      if (sub < sup) "(" + ppe + ")" else ppe
    }
    exp match {
      case Num(int) => int.toString
      case Add(lhs, rhs) => group(1, lhs, '+, rhs) 
      case Mul(lhs, rhs) => group(2, lhs, '*, rhs)
      case Var(sym) => sym.name
    }
  }
}

