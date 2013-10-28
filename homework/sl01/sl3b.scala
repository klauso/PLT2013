object AExp {
  type Env = Map[Symbol, Int]

  abstract class AExp {
    val precedence : Int
    def print : String
    def evaluate(env : Env) : Int
  }

  case class Num(int : Int) extends AExp {
    val precedence : Int = 0
    def print = int.toString
    def evaluate(env : Env) = int
  }

  trait Pretty {
    def pretty(sup : Int, lhs : AExp, bop : Symbol, rhs : AExp) = {
      def group(exp : AExp) = {
        val ppe : String = exp.print
        val sub : Int = exp.precedence
        if (sub < sup) "(" + ppe + ")" else ppe
      }
      group(lhs) + " " + bop.name + " " + group(rhs)
    }
  }

  case class Add(lhs : AExp, rhs : AExp) extends AExp with Pretty {
    val precedence : Int = -2
    def print = pretty(precedence, lhs, '+, rhs)
    def evaluate(env : Env) : Int = lhs.evaluate(env) + rhs.evaluate(env)
  }

  case class Mul(lhs : AExp, rhs : AExp) extends AExp with Pretty {
    val precedence : Int = -1 
    def print = pretty(precedence, lhs, '*, rhs)
    def evaluate(env : Env) : Int = lhs.evaluate(env) * rhs.evaluate(env)
  }

  case class Var(sym : Symbol) extends AExp {
    val precedence : Int = 0
    def print = sym.name
    def evaluate(env : Env) = env(sym)
  }

  // In object-oriented style, extending the language with new data (classes)
  // is easy.
  case class Sub(lhs : AExp, rhs : AExp) extends AExp with Pretty {
    val precedence : Int = -2
    def print = pretty(precedence, lhs, '-, rhs)
    def evaluate(env : Env) : Int = lhs.evaluate(env) - rhs.evaluate(env)
  }

  case class Div(lhs : AExp, rhs : AExp) extends AExp with Pretty {
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

  implicit def toNum(int : Int) : AExp = Num(int)
  implicit def toVar(sym : Symbol) : AExp = Var(sym)
}

