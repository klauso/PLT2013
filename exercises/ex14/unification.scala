import scala.language.implicitConversions

// Exercise 14.2
//
// Type inference for simply typed lambda calculus
//
// We will implement a type inference algorithm described
// in Pierce's "Types and Programming Langauges",
// sections 22.1 to 22.4, pages 317--327.
//
// The algorithm is briefly visible on slides 6 and 10 of
// 26-typeinference_curryhoward.pdf.


// These are a subset of the case classes in the lecture notes
// 05-fae.scala. They implement untyped lambda calculus.

trait Exp

// a variable in lambda calculus
case class Id(name: Symbol) extends Exp {
  override def toString = name.toString
}

// a lambda abstraction without argument type annotation
case class Fun(param: Symbol, body: Exp) extends Exp

// function application
case class App (funExpr: Exp, argExpr: Exp) extends Exp

// implicit conversion for easier creation of variables (Id)
implicit def id2exp(s: Symbol) = Id(s)

// We use case classes to represent types as well.
trait Type

// Type variables are T1, T2, T3, ...
// They are seen on slide 6 of 26-typeinference_curryhoward.pdf.
case class TypeVariable(i: Int) extends Type {
  override def toString = s"T$i"
}

// The mutable object `TypeVariable` can give us 2^31 fresh
// type variables.
var typeVariableCounter: Int = -1
def freshTypeVariable: TypeVariable = {
  typeVariableCounter += 1 ;
  TypeVariable(typeVariableCounter)
}

// The type of functions. The type T1 -> T2 is represented by
// Arrow(TypeVariable(1), TypeVariable(2))
case class Arrow(domain: Type, range: Type) extends Type {
  override def toString = s"($domain -> $range)"
}

// Step 1 of the algorithm is shown in Figure 22-1.
// Let us implement an FAE adaptation.

// Given a term, we return a tentative type for it, along
// with a set of constraints. The tentative type is usually
// just a type variable Ti. The constraints tell us what
// Ti will eventually stand for.

// The class Eq stands for equality constraint between two types.
// Eq(TypeVariable(1), TypeVariable(2)) stands for T1 == T2.
case class Eq(lhs: Type, rhs: Type)

case class ConstrainedType(getType: Type, getConstraints: List[Eq])

// A typing context represents types given to variables bound
// by lambda abstractions outside the current expression.
// It ensures that all occurrences of a variable 'x are given
// the same type. The typing context is usually denoted
// by the Greek letter gamma in typing rules.

type Context = Map[Symbol, Type]

def constrainedTyping(e: Exp, gamma: Context): ConstrainedType =
  e match {
    case Id(x) =>
      // When we encounter an identifier, return the type
      // assigned to it in the context, together with
      // no constraints.
      ConstrainedType(gamma(x), Nil)

    case Fun(x, body) =>
      // When we encounter a lambda abstraction, come up with
      // a fresh type variable to denote the type of x.
      val xType = freshTypeVariable

      // Add the mapping from x to xType into the context.
      //
      // (Here and elsewhere, please replace ??? by an appropriate
      // implementation.)
      val newGamma: Context = ???

      // Obtain the type and constraints of `body` under
      // the new context.
      val bodyConstrainedType: ConstrainedType = ???

      val ConstrainedType(bodyType, bodyConstraints) = bodyConstrainedType

      // Return the constrained type of this lambda abstraction.
      // It should be a function type.
      // We should also save the constraints accumulated from `body`.
      ConstrainedType(???, ???)

    case App(funExpr, argExpr) =>
      // When we encounter an application, generate two type
      // variables, one to represent the type of the argument,
      // one to represent the return type of the function.

      val argType    = freshTypeVariable
      val returnType = freshTypeVariable

      // Obtain the tentative types and constraints of funExpr
      // and argExpr.

      val funConstrainedType: ConstrainedType = ???
      val argConstrainedType: ConstrainedType = ???

      val ConstrainedType(actualFunType, funConstraints) = funConstrainedType
      val ConstrainedType(actualArgType, argConstraints) = argConstrainedType

      // Create two new constraints:
      //
      // 1. The actual argument type matches the type variable
      //    we have assigned to it.
      val newConstraint1 = Eq(argType, actualArgType)

      // 2. The actual type of funExpr matches the expected
      //    function type from argType to returnType.
      val newConstraint2 = Eq(???, ???)

      // The tentative type of this function application is the
      // return type of the function.
      //
      // The constraints associated with this function application
      // consist of the two new constraints, the constraints
      // accumulated from `funExpr`, and the constraints accumulated
      // from `argExpr`.
      ConstrainedType(returnType, ???)
  }

// Step 2 is shown in Figure 22-2.
// It needs a notion of substitution over types,
// as well as a notion of composition of substitutions.

// Substitution over types is easy, because accidental name
// capturing is impossible. You are welcome to think a bit
// about why.
//
// A substitution of type variables can be viewed as
// a transformation between types. TAPL and lecture slides on
// the internet tend to take this view.
type Substitution = Type => Type

// This function creates a substitution about one type
// variable.
def subst(from: TypeVariable, to: Type): Substitution = _ match {
  case ti @ TypeVariable(_) if ti == from =>
    to
  case ti @ TypeVariable(_) if ti != from =>
    ti
  case Arrow(domain, range) =>
    Arrow(subst(from, to)(domain), subst(from, to)(range))
}

// Substitutions can be composed just like functions.
// The effect is rather amazing.

val T0 = freshTypeVariable
val T1 = freshTypeVariable
val T2 = freshTypeVariable

val sigma1 = subst(T1, Arrow(T2, T2))
val sigma0 = subst(T0, Arrow(T1, T2))

val composedSigmas = sigma1 compose sigma0

val type0 = Arrow(T0, T0)

println(composedSigmas(type0))
// (((T2 -> T2) -> T2) -> ((T2 -> T2) -> T2))

// Another helper function computes all type variables
// occurring in a type. It's used in the "occurs" check.
// Since type variables have no binders, it's pretty
// to write.

def freeNames(theType: Type): Set[TypeVariable] = theType match {
  case ti @ TypeVariable(_) =>
    ???
  case Arrow(domain, range) =>
    ???
}

// Armed with `subst` and `compose`, it should be
// possible to write the unification algorithm in
// Figure 2-22. The same algorithm is shown on
// slide 10 of 26-typeinference_curryhoward.pdf.

def unify(constraints: List[Eq]): Substitution = ???


// Armed with unification, it is possible to write
// a function that computes the type of a term.

def getType(exp: Exp): Type = {
  val ConstrainedType(theType, constraints) =
    constrainedTyping(exp, Map.empty)
  val substitution = unify(constraints)
  substitution(theType)
}


// Here's a function that renames type variables
// uniformly so that it's easier to check whether
// your implementation is correct.
def canonize(theType: Type): Type = {
  var i = -1
  val correspondence =
    collection.mutable.Map.empty[TypeVariable, TypeVariable]

  def nextTypeVariable: TypeVariable = {
    i += 1
    TypeVariable(i)
  }

  def loop(theType: Type): Type = theType match {
    case ti @ TypeVariable(_) if correspondence.contains(ti) =>
      correspondence(ti)
    case ti @ TypeVariable(_) if ! correspondence.contains(ti) =>
      val tiCanonized = nextTypeVariable
      correspondence.update(ti, tiCanonized)
      tiCanonized
    case Arrow(domain, range) =>
      Arrow(loop(domain), loop(range))
  }

  loop(theType)
}

// print the canonical type of a term
def printType(name: String, exp: Exp): Unit =
  println(s"$name : ${canonize(getType(exp))}")

// test cases of terms without type annotations
// from homework 11, problem 2

// After the implementation is done, uncomment the lines
// such as `printType(t0)` to test it.

//  t0 : (T0 -> T0)
val t0 = Fun('x, 'x)
//printType("t0", t0)

//  t1 : (T0 -> (T1 -> T0))
val t1 = Fun('x, Fun('y, 'x))
//printType("t1", t1)

//  t2 : (T0 -> ((T0 -> T1) -> T1))"
val t2 = Fun('x, Fun('f, App('f, 'x)))
//printType("t2", t2)

//  t3 : ((T0 -> (T1 -> T2)) -> (T1 -> (T0 -> T2)))
val t3 = Fun('f, Fun('x, Fun('y, App(App('f, 'y), 'x))))
//printType("t3", t3)

//  t4 : ((T0 -> T1) -> ((T1 -> T2) -> (T0 -> T2)))
val t4 = Fun('f, Fun('g, Fun('x, App('g, App('f, 'x)))))
//printType("t4", t4)

//  t5 : ((T0 -> (T1 -> T2)) -> ((T0 -> T1) -> (T0 -> T2)))
val t5 = Fun('x, Fun('y, Fun('z, App(App('x, 'z), App('y, 'z)))))
//printType("t5", t5)
