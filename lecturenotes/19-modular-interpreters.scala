import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls

trait Monad {
  type M[_]
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]

  implicit def monadicSyntax[A](m: M[A]) = new {
    def map[B](f: A => B) = bind(m, (x: A) => unit(f(x)))
    def flatMap[B](f: A => M[B]) : M[B] = bind(m,f)
  }
}  
trait Expressions extends Monad {

  abstract class Value
  
  abstract class Exp {
    def eval : M[Value]
  }  
}

trait Numbers extends Expressions {
  case class NumV(n: Int) extends Value
}

trait Arithmetic extends Numbers {
  case class Num(n: Int) extends Exp {
    def eval = unit(NumV(n))
  }
  implicit def num2exp(n: Int) = Num(n)
  
  case class Add(lhs: Exp, rhs: Exp) extends Exp {
    def eval = for { 
	             l <- lhs.eval 
				 r <- rhs.eval
			   } yield (l,r) match {
			                case (NumV(v1), NumV(v2)) => NumV(v1+v2)
							case _ => sys.error("can only add numbers")
						}
  }		
}

trait If0 extends Numbers {  
  case class If0(cond: Exp, thenExp: Exp, elseExp: Exp) extends Exp {
    def eval = for {
	             c <- cond.eval
	             res <- c match { case NumV(0) => thenExp.eval
	                              case _ => elseExp.eval }
	           } yield res 
  }
}


trait IdentityMonad extends Monad {
  type M[A] = A
  def unit[A](a: A) : M[A] = a
  def bind[A,B](m: M[A], f: A => M[B]) = f(m)
}

object AE extends Arithmetic with IdentityMonad {
  val aetest = Add(1,Add(2,3))
}
assert(AE.aetest.eval == AE.NumV(6)) 

trait ReaderMonad extends Monad {
  type R
  def ask : M[R]
  def local[A](f: R => R, a: M[A]) : M[A] 
}  

trait Functions extends Expressions with ReaderMonad {
  type Env = Map[Symbol,Value]
  override type R = Env

  case class ClosureV(f: Fun, env: Env) extends Value
  case class Fun(param: Symbol, body: Exp) extends Exp {
    def eval = for { env <- ask } yield ClosureV(this, env)
  }	
  case class App(f: Exp, a: Exp) extends Exp {
    def eval = for {
	            fv <- f.eval
				av <- a.eval
				res <- fv match { case ClosureV(fun,cenv) => local( env => cenv + (fun.param -> av), fun.body.eval) }
		       } yield res

  }
  case class Id(x: Symbol) extends Exp {
    def eval = for {
	             env <- ask
			   } yield env(x)
  }
  implicit def id2exp(x: Symbol) = Id(x)
  def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)
}  

  
trait ReaderMonadImpl extends ReaderMonad {
  type M[X] = R => X
  def unit[A](a: A) : M[A] = r => a
  def bind[A,B](m: M[A], f: A => M[B]) : M[B] = r => f(m(r))(r)
  def ask : M[R] = identity
  def local[A](f: R => R, a: M[A]) : M[A] = (r) => a(f(r))
}  

object FAELang extends Functions with Arithmetic with ReaderMonadImpl {
  // type R = Env
  val faetest = App(Fun('x, Add('x, 1)), Add(2,3))
  assert(faetest.eval(Map.empty) == NumV(6))
}

trait StateMonad extends Monad {
  type S
  def getState : M[S]
  def putState(s: S) : M[Unit]
}

trait Boxes extends Expressions with StateMonad  {
  override type S = Store
  type Store = Map[Address,Value]

  type Address = Int
  var _nextAddress = 0

  def nextAddress : Address = {
    _nextAddress += 1
    _nextAddress
  }
  
  case class AddressV(a: Address) extends Value

  case class NewBox(e: Exp) extends Exp {
    def eval = {
	  val a = nextAddress
	  for {
	    v <- e.eval
		s <- getState
		_ <- putState(s + (a -> v))
	  } yield AddressV(a)
    }
  }
  case class SetBox(b: Exp, e: Exp) extends Exp {
     def eval = 
       for {
          box <- b.eval
          ev  <- e.eval
          s   <- getState
          _   <- putState(box match { case AddressV(a) => s.updated(a,ev) })
	   } yield ev
  }
  
  
  case class OpenBox(b: Exp) extends Exp {
    def eval = for {
	             bv <- b.eval
	             s  <- getState
			   } yield (bv match { case AddressV(a) => s(a) })
  }
  case class Seq(e1: Exp, e2: Exp) extends Exp {
    def eval = bind(e1.eval,(_:Value) => e2.eval)
  }
  
}

trait ReaderStateMonadImpl extends ReaderMonad with StateMonad {
  type M[A] = (R,S) => (A,S)
  def unit[A](a: A) : M[A] = (r : R, s: S) => (a,s)
  def bind[A,B](m: M[A], f: A => M[B]) : M[B] = (r:R, s: S) => {
     val (a,s2) = m(r,s)
	 f(a)(r,s2)
  }
  def ask : M[R] = (r,s) => (r,s)
  def local[A](f: R => R, a: M[A]) : M[A] = (r:R,s:S) => a(f(r),s)
  def getState : M[S] = (r,s) => (s,s)
  def putState(s: S) : M[Unit] = (r,_) => ((),s)
}  

object BCFAE extends Boxes with Arithmetic with Functions with If0 with ReaderStateMonadImpl {
  val test = wth('switch, NewBox(0),
                wth('toggle, Fun('dummy, If0(OpenBox('switch),
                                          Seq(SetBox('switch, 1), 1),
                                          Seq(SetBox('switch, 0), 0))),
                             Add(App('toggle,42), App('toggle,42))))  
}  

assert(BCFAE.test.eval(Map.empty,Map.empty)._1 == BCFAE.NumV(1))

trait ContinuationMonad extends Monad {
  def callcc[A,B](f : (A => M[B]) => M[A]) : M[A]
}

def continuationMonad[R] = new ContinuationMonad {
  type M[A] = (A => R) => R
  override def unit[A](a: A) : M[A] = k => k(a)
  override def bind[A,B](m: M[A], f: A => M[B]): M[B] = k => m( a => f(a)(k))
  override def callcc[A,B](f : (A => (B => R) => R) => (A => R) => R) : (A => R) => R = k => f(a => _ => k(a))(k)
}

trait ContinuationReaderMonad extends ReaderMonad with ContinuationMonad {
  type R
  type T
  type M[A] = R => (A => T) => T
  override def unit[A](a: A) = r => k => k(a)
  override def bind[A,B](m: M[A], f: A => M[B]) = r => k => m(r)(a => f(a)(r)(k))
  override def ask : M[R] = r => k => k(r)
  override def local[A](f: R => R, a: M[A]) : M[A] = r => k => a(f(r))(k)
  override def callcc[A,B](f : (A => M[B]) => M[A]) : M[A] = r => k => f(a => r2 => k2 => k(a))(r)(k)
}  

trait Letcc extends Expressions with ContinuationMonad with ReaderMonad{
  override type R = Map[Symbol,Value]
  
  case class CApp(f: Exp, a: Exp) extends Exp {
    override def eval : M[Value] = 
        for {
           fv <- f.eval
           av <- a.eval
           res <- fv match { case ContV(f) => f(av) }
         } yield res
  }
  case class Letcc(param: Symbol, body: Exp) extends Exp {
    override def eval : M[Value] = callcc[Value,Value](k => local( env => env + (param -> ContV(k)), body.eval))
  }  
  case class ContV(f: Value => M[Value]) extends Value
}

object FAEwLetcc extends Arithmetic with Functions with If0 with Letcc with ContinuationReaderMonad {
  override type R = Env
  override type T = Value
  val testprog = Add(1, Letcc('k, Add(2, CApp('k, 3))))  
}  

assert(FAEwLetcc.testprog.eval(Map.empty)(identity) == FAEwLetcc.NumV(4))