. Examples of programming in System F
. based on §23.4 of "Types and Programming Languages"
. by Benjamin C. Pierce

. Type :help into the prompt for a description of commands.
. Type :grammar to read the grammar of System F.
. Type :typing to read the typing rules of System F.

. These expressions can be keyed into the prompt:
.
. + 2 2
. succ = \abs x : Int. + x 1
. succ 5
. succ 99

. Type variables has to be bound by a type abstraction:
.
. > id = \abs x : a. x
. λx : a. x
.      ^
. undeclared type variable a

id = \Tabs a. \abs x : a. x

. Try inputing these terms:
.
. id 5
. id-int = id [Int]
. id-int 5
. id [Int] 5
.
. (The point is, a polymorphic function has to be
. instantiated by a type application before calling it
. on an argument.)

double = \Tabs a. \abs f : a -> a. \abs x : a. f (f x)

. double-int = double [Int]
. double-fun = double [Int -> Int]
. double-int (λx : Int. succ (succ x)) 3

plus4 = double [Int -> Int] (double [Int]) (+ 1)

. Stepping through the reduction sequence of `plus4 7`
. may be instructive.
.
. :step plus4 7

. It's possible to call a polymorphic function on itself.
self-app = \abs x : \all a. a -> a. x [\all b. b → b] x

. The :primitives command lists available primitives,
. among them the list constructors `cons` and `nil`.
.
. :primitives

list1234 =
  cons [Int] 1 (cons [Int] 2 (cons [Int] 3 (cons [Int] 4 (nil [Int]))))

. isnil (on nil & list1234)
. head list1234
. tail list1234

. Pure System F has no recursion.
. Our impelementation permits it, however.
. We can use recursion to write a function `map` that applies
. another function to every element of a list.

map : \all a b. (a -> b) -> List a -> List b

map =  \Tabs a b.
       \abs f : a -> b.
       \abs xs : List a.

         . apply f to every element in xs

         . condition
         isnil [a] xs [List b]

           . then-branch
           (nil [b])

           . else-branch
           (cons [b]
             (f (head [a] xs))
             (map [a] [b] f (tail [a] xs)))

. map [Int] [Int] plus4 list1234

. Polymorphic terms are well-suited to encoding data structures.
. A Church-encoded data type is its own "elimination form";
. Church Booleans act like if-then-else statements,
. Church numerals act like for-loops.

type CBool = \all a. a -> a -> a

. Convert a Church Boolean to a Boolean literal
. so that we can see easily what it is.
to-bool : CBool -> Bool
to-bool = \abs b : CBool. b [Bool] true false

tru = \Tabs a. \abs then : a. \abs else : a. then
fls = \Tabs a. \abs then : a. \abs else : a. else

not = \abs b : CBool.
  \Tabs a. \abs then : a. \abs else : a.
    b [a] else then

. Exercise: write a function `and` such that:
.
. to-bool (and fls fls) = false
. to-bool (and fls tru) = false
. to-bool (and tru fls) = false
. to-bool (and tru tru) = true

and : CBool -> CBool -> CBool
and = ??? [CBool -> CBool -> CBool]

. Exercise: write a function `or` such that:
.
. to-bool (or fls fls) = false
. to-bool (or fls tru) = true
. to-bool (or tru fls) = true
. to-bool (or tru tru) = true

or : CBool -> CBool -> CBool
or = ??? [CBool -> CBool -> CBool]

. As remarked above, Church numerals are similar
. to for-loops. The Church numeral `n` applies 
. a function `n` times to an argument.

type CNat = \all a. (a -> a) -> a -> a

. Applying the function `s` zero times means not
. applying it at all.
zero = \Tabs a. \abs s : a -> a. \abs z : a. z

. Applying `s` once.
one = \Tabs a. \abs s : a -> a. \abs z : a. s z

. Applying `s` twice.
two = \Tabs a. \abs s : a -> a. \abs z : a. s (s z)

. Applying `s` three times.
three = \Tabs a. \abs s : a -> a. \abs z : a. s (s (s z))

. Convert a Church numeral to an integer to print it.
to-int = \abs n : CNat. n [Int] (+ 1) 0

. The successor function: given a Church numeral,
. produce another one that loops one more time.
csucc : CNat → CNat
csucc = \abs n : CNat.
  \Tabs a. \abs s : a → a. \abs z : a. s (n [a] s z)

four = csucc three
five = csucc four

. Addition
c+ : CNat → CNat → CNat
c+ = \abs m : CNat. \abs n : CNat. m [CNat] csucc n

. Addition without calling `succ`
c+' = \abs m : CNat. \abs n : CNat.
         \Tabs a. \abs s : a -> a. \abs z : a. m [a] s (n [a] s z)

. Multiplication
c* = \abs m : CNat. \abs n : CNat. m [CNat] (c+ n) zero

. Multiplication not in terms of `c+`
c*' = \abs m : CNat. \abs n : CNat. \Tabs a. \abs s : a -> a.
  n [a] (m [a] s)

. Exponentiation
c^ = \abs m : CNat. \abs n : CNat. n [CNat] (c* m) one

. Exercise: write an exponentiation function without using
. csucc, c+ or c*. If you saw the answer in the text book
. already, then please write it down and explain why it works.

c^' : CNat -> CNat -> CNat
c^' = ??? [CNat -> CNat -> CNat]
