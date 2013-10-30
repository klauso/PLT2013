object AExp {
  type Env = Map[Symbol, Int]

  abstract class Exp {
    val precedence : Int
    def print : String
    def evaluate(env : Env) : Int
  }

  case class Num(int : Int) extends Exp {
    val precedence : Int = 0
    def print = int.toString
    def evaluate(env : Env) = int
  }

  trait Pretty {
    def pretty(sup : Int, lhs : Exp, bop : Symbol, rhs : Exp) = {
      def group(exp : Exp) = {
        val ppe : String = exp.print
        val sub : Int = exp.precedence
        if (sub < sup) "(" + ppe + ")" else ppe
      }
      group(lhs) + " " + bop.name + " " + group(rhs)
    }
  }

  case class Add(lhs : Exp, rhs : Exp) extends Exp with Pretty {
    val precedence : Int = -2
    def print = pretty(precedence, lhs, '+, rhs)
    def evaluate(env : Env) : Int = lhs.evaluate(env) + rhs.evaluate(env)
  }

  case class Mul(lhs : Exp, rhs : Exp) extends Exp with Pretty {
    val precedence : Int = -1 
    def print = pretty(precedence, lhs, '*, rhs)
    def evaluate(env : Env) : Int = lhs.evaluate(env) * rhs.evaluate(env)
  }

  case class Var(sym : Symbol) extends Exp {
    val precedence : Int = 0
    def print = sym.name
    def evaluate(env : Env) = env(sym)
  }

  // In object-oriented style, extending the language with new data (classes)
  // is easy.
  case class Sub(lhs : Exp, rhs : Exp) extends Exp with Pretty {
    val precedence : Int = -2
    def print = pretty(precedence, lhs, '-, rhs)
    def evaluate(env : Env) : Int = lhs.evaluate(env) - rhs.evaluate(env)
  }

  case class Div(lhs : Exp, rhs : Exp) extends Exp with Pretty {
    val precedence : Int = -1 
    def print = pretty(precedence, lhs, '/, rhs)
    def evaluate(env : Env) : Int = {
      val divisor : Int = rhs.evaluate(env)
      if (divisor == 0)
        sys.error("Dividing by zero: " + this.print)
      else
        lhs.evaluate(env) / divisor
    }
  }

  implicit def toNum(int : Int) : Exp = Num(int)
  implicit def toVar(sym : Symbol) : Exp = Var(sym)
}

