import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls


// This is a library of monads and monad transformers.
// We start with interfaces for the various monads.
// These interfaces leave the type constructor M[_] abstract.
// This is important, because M[_] depends on the concrete
// composition of monads at hand.

// The base Monad interface
trait Monad {
  type M[_] // treating the type constructor as abstract type and not type parameter
            // enables shorter type signatures and more precise typing
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]

  // This part enables the use of for comprehension syntax for monads.
  implicit def monadicSyntax[A](m: M[A]) = new {
    def map[B](f: A => B) = bind(m, (x: A) => unit(f(x)))
    def flatMap[B](f: A => M[B]) : M[B] = bind(m,f)
  }

  // The "monad laws":
  // 1) "unit" acts as a kind of neutral element of "bind", that is:
  //    1a) bind(unit(x),f) == f(x) and
  //    1b) bind(x, y => unit(y)) == x
  // 2) Bind enjoys an associative property
  //     bind(bind(x,f),g) == bind(x, y => bind(f(y),g))
  
}  

// The reader monad is parametric in the "environment" type R.
// The ask function is for retrieving the environment, the local
// function for local changes to the environment.
trait ReaderMonad extends Monad {
  type R
  def ask : M[R]
  def local[A](f: R => R, a: M[A]) : M[A] 
}  

// The state monad is parametric in the state type S.
// The current state can be retrieved with getState.
// The current state can be modified with putState.
trait StateMonad extends Monad {
  type S
  def getState : M[S]
  def putState(s: S) : M[Unit]
}

// The continuation monad provides a method callcc,
// which reifies the current continuation k : A => M[B]
trait ContinuationMonad extends Monad {
  def callcc[A,B](f : (A => M[B]) => M[A]) : M[A]
}

// End of Monad Interfaces

// Now we provide implementations of the monad interfaces.

// The identity monad, which is the end of each transformer chain
trait IdentityMonad extends Monad {
  type M[A] = A
  def unit[A](a: A) : M[A] = a
  def bind[A,B](m: M[A], f: A => M[B]) = f(m)
}

object IdentityMonad extends IdentityMonad

// We organize most other monads as monad _transformers_.
// A monad transformer is parameterized with another monad.
// The monads are organized in a chain. Operations of 
// "inner" monads must be lifted to top-level operations.

trait MonadTransformer extends Monad {
  protected val m : Monad
}  

// The Reader monad transformer. We provide some convenient
// functions lift, lift2 etc. to lift functions from the inner monad.
// Note that M[X] = R => m.M[X] instead of M[X] = R => X (as for
// the non-transformer version of the reader monad).
// The correct implementation of the interface methods follows from
// this type equation.

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

// The original Reader monad can be reconstructed by composing ReaderT with the identity monad.

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

// The design of StateT is similar to that of ReaderT
trait StateT extends MonadTransformer with StateMonad {
  type M[A] = S => m.M[(A,S)]
  override def unit[A](a: A) : M[A] = (s: S) => m.unit(a,s)
  override def bind[A,B](x: M[A], f: A => M[B]) : M[B] = (s: S) => {
     m.bind[(A,S),(B,S)](x(s), { case (a,s2) => f(a)(s2)})
  }
  override def getState : M[S] = s => m.unit((s,s))
  override def putState(s: S) : M[Unit] = _ => m.unit(((),s))
}

// and again we can reconstruct the ordinary state monad.

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
// But for simplicity we only present the ordinary 
// continuation monad here.

trait ContinuationMonadImpl extends ContinuationMonad {
  type T
  type M[A] = (A => T) => T
  override def unit[A](a: A) : M[A] = k => k(a)
  override def bind[A,B](m: M[A], f: A => M[B]): M[B] = k => m( a => f(a)(k))
  override def callcc[A,B](f : (A => (B => T) => T) => (A => T) => T) : (A => T) => T = k => f(a => _ => k(a))(k)
}

// The composition of the Reader monad and some continuation monad.
trait ReaderContinuationMonadForwarder extends ReaderT with ContinuationMonad {
  val m : ContinuationMonad
  override def callcc[A,B](f : (A => M[B]) => M[A]) : M[A] = (m.callcc[A,B] _)(f) // call to lift4 inserted automatically                                                                                    
}

// The composition of the Reader monad and the continuation monad implementation.
trait ReaderContinuationMonadImpl extends ReaderContinuationMonadForwarder {
  type T
  val m = new ContinuationMonadImpl { type T = ReaderContinuationMonadImpl.this.T }
}  

// Composition of reader monad with some state monad.
trait ReaderStateMonadForwarder extends ReaderT with StateMonad {
  val m: StateMonad { type S = ReaderStateMonadForwarder.this.S }
  override def getState : M[S] = m.getState
  override def putState(s: S) : M[Unit] = m.putState(s)
}

// Composition of reader monad with StateMonadImpl
trait ReaderStateMonadImpl extends ReaderStateMonadForwarder {
  val m = new StateMonadImpl { type S = ReaderStateMonadImpl.this.S }
}