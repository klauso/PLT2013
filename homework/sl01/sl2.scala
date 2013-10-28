object AExp {
  type Env = Map[Symbol, Int]

  abstract class AExp {
    val precedence : Int
    def print : String
    // In object-oriented style, extending the language with new operations
    // causes changes to all existing data (class).
    def evaluate(env : Env) : Int
  }

  case class Num(int : Int) extends AExp {
    val precedence : Int = 0
    def print = int.toString
    // caused change
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
    // caused change
    def evaluate(env : Env) : Int = lhs.evaluate(env) + rhs.evaluate(env)
  }

  case class Mul(lhs : AExp, rhs : AExp) extends AExp with Pretty {
    val precedence : Int = -1 
    def print = pretty(precedence, lhs, '*, rhs)
    // caused change
    def evaluate(env : Env) : Int = lhs.evaluate(env) * rhs.evaluate(env)
  }

  case class Var(sym : Symbol) extends AExp {
    val precedence : Int = 0
    def print = sym.name
    // caused change
    def evaluate(env : Env) = env(sym)
  }

  implicit def toNum(int : Int) : AExp = Num(int)
  implicit def toVar(sym : Symbol) : AExp = Var(sym)
}

