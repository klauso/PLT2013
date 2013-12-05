/**
* These are lecture notes for the "Programming Languages and Types" course by
* Klaus Ostermann at the University of Marburg
*/
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.reflectiveCalls

/* 
We have seen various patterns of function composition:
 - The environment passing style, in which an environment
   is passed down in recursive calls
 - The store passing style, in which a store is threaded 
   in and out of every computation
 - The continuation passing style, in which every function
   call is a tail call.

Monads are way to abstract over such patterns of function composition. 
Using monads, we can write code which can be parameterized to be in 
one of the styles above (or many others).

Here is another common pattern of function composition. Suppose we have
the following API:
*/
def f(n: Int) : String = sys.error("not implemented")
def g(x: String) : Boolean = sys.error("not implemented")
def h(b: Boolean) : Int = sys.error("not implemented")

def clientCode = h(!g(f(27)+"z"))

/* Now suppose that these functions can possibly fail (say, because
they involve remote communication). A common way to deal with such 
failures is to use the Option datatype: */

def f(n: Int) : Option[String] = sys.error("not implemented")
def g(x: String) : Option[Boolean] = sys.error("not implemented")
def h(b: Boolean) : Option[Int] = sys.error("not implemented")

/* However, now the clientCode must be changed rather dramatically: */

def clientCode = 
  f(27) match {
    case Some(x) => g(x+"z") match {
	                     case Some(y) => h(!y)
						 case None => None
					}
	case None => None
  }
 
/* We see a kind of pattern in this code. We have a value of type
Option[A], but the next function we need to call requires an A and
produces an Option[B]. If the Option[A] value is None, then the whole
computation produces None. If it is Some(x) instead, we pass x to
the function.

We can capture this pattern in the form of a function: */

def bindOption[A,B](a: Option[A], f: A => Option[B]) : Option[B] = a match {
    case Some(x) => f(x)
	case None => None
}	

/* Using bindOption, we can rewrite the code above as follows: */

def clientCode =
  bindOption(f(27), (x:String) => 
      bindOption(g(x+"z"), (y:Boolean) =>
	    h(!y)))
		
/* The nested bindOption calls make the code look rather heavyweight. 
Let's use some standard Scala tricks that allow us to use bindOption
as an infix operator called ">>=".  
*/

implicit def monadicSyntax[A](m: Option[A]) = new {
   def bind[B](f: A => Option[B]) : Option[B] = bindOption(m,f)
}

// Using "bind", we can rewrite the code from above as follows:
	
def clientCode =
  f(27) bind ((x: String) =>
  g(x+"z") bind  ((y: Boolean) =>
  h(!y)))

/* Now suppose that our original client code was not h(!g(f(27)+"z")) 
but instead !g(f(27)+"z"). How can we express this with bind? This
thing does not type check:

def clientCode =
  f(27) bind ((x: String) =>
  g(x+"z") bind  ((y: Boolean) =>
  !y))

One way to fix the situation is to insert a call to Some, like so:  
*/
def clientCode =
  f(27) bind ((x: String) =>
  g(x+"z") bind  ((y: Boolean) =>
  Some(!y)))

/* While this works, it is incompatible with our original goal of abstracting
over the function composition pattern, because the Some constructor exposes what
kind of pattern we are currently dealing with. Hence let's abstract over
it by adding a second function "unit" to our function composition interface: */

def unit[A](x: A) : Option[A] = Some(x)

def clientCode =
  f(27) bind ((x: String) =>
  g(x+"z") bind  ((y: Boolean) =>
  unit(!y)))
  
/* This looks better, but the types of unit and bind still reveal that we are
dealing with the "Option" function composition pattern. Let's abstract over
the Option type constructor by turning the type constructor into a parameter.
The resulting triple (type constructor, unit function, bind function) is called
a _monad_. Certain conditions on unit and bind also need to hold to make it
a true monad, but we'll defer a discussion of these conditions until later.

So here it is: The Monad interface. */

trait Monad[M[_]] {
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]
}

/* Using this interface, we can now make clientCode depend only on this interface,
but no longer on the Option type: */

def clientCode(m: Monad[Option]) =
  m.bind(f(27),  (x: String) =>
  m.bind(g(x+"z"),  (y: Boolean) =>
  m.unit(!y)))
  
/* If the API is parametric in the monad, we can make the client code fully
parametric, too. We model the monad object as an implicit parameter to save
the work of passing on the monad in every call. */
  
def f[M[_]](n: Int)(implicit m: Monad[M]) : M[String] = sys.error("not implemented")
def g[M[_]](x: String)(implicit m: Monad[M]) : M[Boolean] = sys.error("not implemented")
def h[M[_]](b: Boolean)(implicit m: Monad[M]) : M[Int] = sys.error("not implemented")

def clientCode[M[_]](implicit m: Monad[M]) =
  m.bind(f(27),  (x: String) =>
  m.bind(g(x+"z"),  (y: Boolean) =>
  m.unit(!y)))

/* We have of course already seen one particular Monad: The Option monad. 
This monad is also sometimes called the Maybe monad. 
*/

object OptionMonad extends Monad[Option] {
  override def bind[A,B](a: Option[A], f: A => Option[B]) : Option[B] =
    a match {
      case Some(x) => f(x)
	  case None => None
    }
  override def unit[A](a: A) = Some(a)
}	

/* We can now parameterize clientCode with OptionMonad. */

def v : Option[Boolean] = clientCode(OptionMonad)  

/* There are many other sensible monads we could parameterize clientCode
with. Before we discuss those, let us discuss whether there are useful
functions that are generic enough to be useful for many different monads.
Here are some of these functions: */

// fmap turns every function between A and B into a function between M[A] and M[B]
def fmap[M[_],A,B](f: A => B)(implicit m: Monad[M]): M[A] => M[B] = a => m.bind(a,(x:A) => m.unit(f(x)))
// In fancy category theory terms, we can say that every monad is a functor.

def sequence[M[_],A](l: List[M[A]])(implicit m: Monad[M]) : M[List[A]] = l match {
  case x :: xs => m.bind(x, (y: A) => 
                  m.bind(sequence(xs), (ys : List[A]) =>
				  m.unit(y :: ys)))
  case Nil => m.unit(List.empty)
}  

def mapM[M[_],A,B](f : A => M[B], l: List[A])(implicit m: Monad[M]) : M[List[B]] =
  sequence(l.map(f))

  
/* Here are some other common monads: */


object IdentityMonad extends Monad[({type M[A] = A})#M] {
  def bind[A,B](x: A, f: A => B) : B = f(x) // pass the "environment" r into both computations
  def unit[A](a: A) : A = a
}

// This is the Reader monad, a.k.a. Environment monad.
// It captures the essence of "environment passing style".

// The type parameter ({type M[A] = R => A})#M looks complicated, but
// it is merely "currying" the function arrow type constructor.
// The type constructor which is created here is M[A] = R => A
def readerMonad[R] = new Monad[({type M[A] = R => A})#M] {
  def bind[A,B](x: R => A, f: A => R => B) : R => B = r => f(x(r))(r) // pass the "environment" r into both computations
  def unit[A](a: A) : R => A = (_) => a
}
 
// The State monad, in which computations depend on a state S which is threaded through the computations 
def stateMonad[S] = new Monad[({type M[A] = S => (A,S)})#M] {
  def bind[A,B](x: S => (A,S), f: A => S => (B,S)) : S => (B,S) = s => x(s) match { case (y,s2) => f(y)(s2) } // thread the state through the computations
  def unit[A](a: A) : S => (A,S) = s => (a,s)
}

// The List monad, in which computations produce lists of results. The bind operator combines all those results in a single list.
object ListMonad extends Monad[List] {
  def bind[A,B](x: List[A], f: A => List[B]) : List[B] = x.flatMap(f) // apply f to each element, concatenate the resulting lists
  def unit[A](a: A) = List(a)
}  

// The Continuation monad, in which computations require a continuation.
def continuationMonad[R] = new Monad[({type M[A] = (A => R) => R})#M] {
  def bind[A,B](x: (A => R) => R, f: A => (B => R) => R) : (B => R) => R = 
     k => x( a => f(a)(k)) // construct continuation for x that calls f with the result of x               
  def unit[A](a: A) : (A => R) => R = k => k(a)
}

trait IOMonad {
  type IO[_]
  def unit[A](a: A): IO[A]
  def bind[A,B](m: IO[A], f: A => IO[B]): IO[B]
  def printString(s: String) : IO[Unit]
  def inputString : IO[String]
  
  def performIO[A](action: IO[A]) : A
}  

val iomonad : IOMonad = new IOMonad {
  type World = String
  type IO[A] = World => (A,World)
  def unit[A](a: A): IO[A] = w => (a,w)
  def bind[A,B](m: IO[A], f: A => IO[B]): IO[B] = w => m(w) match { case (a,w2) => f(a)(w2) }
  def printString(s: String) : IO[Unit] = w => { println(s); ((),w +s+" was printed and then ...\n") }
  def inputString : IO[String] = w => { val input = readLine; (input, w + input+" was entered and then ...\n") }
  
  def performIO[A](action: IO[A]) : A = action("The world in which nothing has happened yet, but then ...\n") match {
    case (a,w) => println("Peformed all actions. The world in which all this happened is: \n"+w); a }
}  

def someIOActions(implicit m: IOMonad) : m.IO[Unit] = 
  m.bind(m.printString("Enter your first name:"), (_:Unit) =>
  m.bind(m.inputString, (firstName : String) => 
  m.bind(m.printString("Enter your last name:"), (_:Unit) => 
  m.bind(m.inputString, (lastName: String) => 
  m.printString("Hello, "+firstName + " " + lastName + "!")))))
  
def test = iomonad.performIO(someIOActions(iomonad))  
  