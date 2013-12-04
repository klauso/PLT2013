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

trait Monad {
  type M[_]
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]
}

/* Using this interface, we can now make clientCode depend only on this interface,
but no longer on the Option type: */

def clientCode(m: Monad { type M[A] = Option[A] }) =
  m.bind(f(27),  (x: String) =>
  m.bind(g(x+"z"),  (y: Boolean) =>
  m.unit(!y)))
  
/* If the API is parametric in the monad, we can make the client code fully
parametric, too. We model the monad object as an implicit parameter to save
the work of passing on the monad in every call. */
  
def f(n: Int)(implicit m: Monad) : m.M[String] = sys.error("not implemented")
def g(x: String)(implicit m: Monad) : m.M[Boolean] = sys.error("not implemented")
def h(b: Boolean)(implicit m: Monad) : m.M[Int] = sys.error("not implemented")

def clientCode(implicit m: Monad) =
  m.bind(f(27),  (x: String) =>
  m.bind(g(x+"z"),  (y: Boolean) =>
  m.unit(!y)))

/* We have of course already seen one particular Monad: The Option monad. 
This monad is also sometimes called the Maybe monad. 
*/

object OptionMonad extends Monad {
  type M[A] = Option[A]
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
def fmap[A,B](f: A => B)(implicit m: Monad): m.M[A] => m.M[B] = a => m.bind(a,(x:A) => m.unit(f(x)))
// In fancy category theory terms, we can say that every monad is a functor.

def sequence[A](m: Monad)(l: List[m.M[A]]) : m.M[List[A]] = l match {
  case x :: xs => m.bind(x, (y: A) => 
                  m.bind(sequence(m)(xs), (ys : List[A]) =>
				  m.unit(y :: ys)))
  case Nil => m.unit(List.empty)
}  

def mapM[A,B](m: Monad)(f : A => m.M[B], l: List[A]) : m.M[List[B]] =
  sequence(m)(l.map(f))

/*
trait Monad {
  type M[_]
  def unit[A](a: A): M[A]
  def bind[A,B](m: M[A], f: A => M[B]): M[B]

  implicit def monadicSyntax[A](m: M[A]) = new {
    def map[B](f: A => B) = bind(m, (x: A) => unit(f(x)))
    def flatMap[B](f: A => M[B]) : M[B] = bind(m,f)
  }
}  
*/