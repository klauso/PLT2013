import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls

// The Monad interfaces
trait Monad {
  type M[_] // treating the type constructor as abstract type and not type parameter
            // enables shorter type signatures and more precise typing
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]

  implicit def monadicSyntax[A](m: M[A]) = new {
    def map[B](f: A => B) = bind(m, (x: A) => unit(f(x)))
    def flatMap[B](f: A => M[B]) : M[B] = bind(m,f)
  }
}  

trait ReaderMonad extends Monad {
  type R
  def ask : M[R]
  def local[A](f: R => R, a: M[A]) : M[A] 
}  

trait StateMonad extends Monad {
  type S
  def getState : M[S]
  def putState(s: S) : M[Unit]
}

trait ContinuationMonad extends Monad {
  def callcc[A,B](f : (A => M[B]) => M[A]) : M[A]
}

// End of Monad Interfaces

// The identity monad, which is the end of each transformer chain
trait IdentityMonad extends Monad {
  type M[A] = A
  def unit[A](a: A) : M[A] = a
  def bind[A,B](m: M[A], f: A => M[B]) = f(m)
}

object IdentityMonad extends IdentityMonad


trait MonadTransformer extends Monad {
  protected val m : Monad
}  

trait ReaderT extends MonadTransformer with ReaderMonad {
  type R
  override type M[X] = R => m.M[X]
  override def unit[A](a: A) : M[A] = r => m.unit(a)
  override def bind[A,B](x: M[A], f: A => M[B]) : M[B] = r => m.bind(x(r), (n:A) => f(n)(r))
  override def ask : M[R] = r => m.unit(r)
  override def local[A](f: R => R, a: M[A]) : M[A] = r => a(f(r))
  protected implicit def lift[A](x: m.M[A]) : M[A] = r => x
  protected implicit def lift2[A,B](x: A => m.M[B]) : A => M[B] = a => lift(x(a))
  protected implicit def lift3[A,B,C](x: (A => m.M[B]) => m.M[C]) : (A => M[B]) => M[C] = f => r => x( (a: A) => f(a)(r)) 
  protected implicit def lift4[A,B,C,D](x: ((A => m.M[B]) => m.M[C]) => m.M[D]) : ((A => M[B]) => M[C]) => M[D] = f => r => x( (a: A => m.M[B]) => f(lift2(a))(r)) 
}

trait ReaderMonadImpl extends ReaderT {
  val m = IdentityMonad
}

/* We do not need this because we have just synthesized it.
trait ReaderMonadImpl extends ReaderMonad {
  type M[X] = R => X
  def unit[A](a: A) : M[A] = r => a
  def bind[A,B](m: M[A], f: A => M[B]) : M[B] = r => f(m(r))(r)
  def ask : M[R] = identity
  def local[A](f: R => R, a: M[A]) : M[A] = (r) => a(f(r))
}  
*/
trait StateT extends MonadTransformer with StateMonad {
  type M[A] = S => m.M[(A,S)]
  override def unit[A](a: A) : M[A] = (s: S) => m.unit(a,s)
  override def bind[A,B](x: M[A], f: A => M[B]) : M[B] = (s: S) => {
     m.bind[(A,S),(B,S)](x(s), { case (a,s2) => f(a)(s2)})
  }
  override def getState : M[S] = s => m.unit((s,s))
  override def putState(s: S) : M[Unit] = _ => m.unit(((),s))
}

trait StateMonadImpl extends StateT {
  val m = IdentityMonad
}  

/* We do not need this because we have just synthesized it.
trait StateMonadImpl extends StateMonad {
  type M[A] = S => (A,S)
  def unit[A](a: A) : M[A] = (s: S) => (a,s)
  def bind[A,B](m: M[A], f: A => M[B]) : M[B] = (s: S) => {
     val (a,s2) = m(s)
	 f(a)(s2)
  }
  def getState : M[S] = s => (s,s)
  def putState(s: S) : M[Unit] = _ => ((),s)
}*/


// We could also synthesize ContinuationMonadImpl from a ContT
// just as we did for ReaderMonadImpl and StateMonadImpl

trait ContinuationMonadImpl extends ContinuationMonad {
  type T
  type M[A] = (A => T) => T
  override def unit[A](a: A) : M[A] = k => k(a)
  override def bind[A,B](m: M[A], f: A => M[B]): M[B] = k => m( a => f(a)(k))
  override def callcc[A,B](f : (A => (B => T) => T) => (A => T) => T) : (A => T) => T = k => f(a => _ => k(a))(k)
}


trait ReaderContinuationMonadForwarder extends ReaderT with ContinuationMonad {
  val m : ContinuationMonad
  override def callcc[A,B](f : (A => M[B]) => M[A]) : M[A] = (m.callcc[A,B] _)(f) // call to lift4 inserted automatically                                                                                  // doesn't work with m.callcc[A,B](f), unfortunately
}

trait ReaderContinuationMonadImpl extends ReaderContinuationMonadForwarder {
  type T
  val m = new ContinuationMonadImpl { type T = ReaderContinuationMonadImpl.this.T }
}  

trait ReaderStateMonadForwarder extends ReaderT with StateMonad {
  val m: StateMonad { type S = ReaderStateMonadForwarder.this.S }
  override def getState : M[S] = m.getState
  override def putState(s: S) : M[Unit] = m.putState(s)
}

trait ReaderStateMonadImpl extends ReaderStateMonadForwarder {
  val m = new StateMonadImpl { type S = ReaderStateMonadImpl.this.S }
}


