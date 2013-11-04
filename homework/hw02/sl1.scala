def mapInts(f : Int => Int, xs : List[Int]) =
  xs.foldRight[List[Int]](Nil)((x : Int, z : List[Int]) => f(x) :: z)

def filterInts(p : Int => Boolean, xs : List[Int]) =
  xs.foldRight[List[Int]](Nil)(
    (x : Int, z : List[Int]) => if (p(x)) x :: z else z
  )

/* polymorphic version
def map[A, B](f : A => B, xs : List[A]) =
  xs.foldRight[List[B]](Nil)((x : A, z : List[B]) => f(x) :: z)

def filter[A](p : A => Boolean, xs : List[A]) =
  xs.foldRight[List[A]](Nil)((x : A, z : List[A]) => if (p(x)) x :: z else z)
*/

