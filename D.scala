import D._;

// The syntax sugar `wth` may eventually prove useful.
def wth(x: Symbol, xdef: Exp, body: Exp) : Exp = App(Fun(x,body),xdef)

/*
D. Implementing RCFAE in FAE
============================

It turns out FAE is so powerful that we can implement Letrec-
expressions as syntactic sugar. Let's have the FAE interpreter in
place first.
*/

// DEFINITIONS
//
// For brevity, we will use the lambda-calculus syntax when discussing
// FAE terms. We write
//
//     lambda x. body     for     Fun('x, body),
//     exp1 exp2          for     App(exp1, exp2),
//
// and we consider the binding between a function and its (one)
// argument to be tighter than the operators + or *; so for example
// f n + 1 means (f n) + 1.
//
// Suppose f is a Fun-expression and f = (lambda x. body).
// Then f and the term (lambda y. f y) behave identically on all
// arguments as long as y is not free in body. We say that the terms
//
//     f     and     (lambda y. f y)
//
// are eta-equivalent. In addition, two terms are eta-equivalent
// if they have the same constructor and all their subterms are
// eta-equivalent.


// a) Observe the Z-combinator below.
//    Prove that for every Fun-expression f, the value of (Z f) is
//    eta-equivalent to f (Z f).
//
//    If we equate eta-equivalent terms, then what we want to prove
//    can be written as
//
//        Z f == f (Z f).
//
//    That is why Z is called a "fixed-point combinator". More on the
//    topic:
//
//        http://en.wikipedia.org/wiki/Fixed-point_combinator

//    Z = lambda f.
//          (lambda x. f (lambda y. x x y))
//          (lambda x. f (lambda y. x x y))

// Note the verbatim string literal!
val Z = D.parse("""\f. (\x. f \y. x x y) (\x. f \y. x x y)""")


// b) Prove that if a function f is a fixed point of the higher-order
//    function phi_of_factorial below, then (f n) computes the
//    factorial n. In other words, prove that
//
//        f == phi_of_factorial f
//
//    implies
//
//        f 0 == 1,
//        f n == n * f (n - 1).

//    phi_of_factorial = lambda f.
//                         lambda x. If0(x, 1, x * f(x - 1))

val phi_of_factorial = D.parse("""\f. \x. ? x | 1 : x * (x + -1)""")


// c) Given a) and b), write down the factorial function as a FAE
// term.

// val factorial = ...

// A test. Uncomment to run.

/*
def call_fact(n: Int) : Int = eval(App(factorial, n)) match {
  case NumV(m) => m ; case _ => sys.error("Bug in factorial!")
}
assert(
List(0, 1, 2, 3,  4,   5,   6,    7,     8,      9).map(call_fact) ==
List(1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880))
*/


// d) Write down a FAE term `fib` such that (fib n) computes the nth
//    fibonnacci number.



// A test. Uncomment to run.

/*
def call_fib(n: Int) : Int = eval(App(fib, n)) match {
  case NumV(m) => m ; case _ => sys.error("Bug in fib!")
}
assert(
List(0, 1, 2, 3, 4, 5,  6,  7,  8,  9).map(call_fib) ==
List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
*/


// e) Write down letrec as syntactic sugar of FAE.

def letrec(x: Symbol, xdef: Exp, body: Exp) : Exp
  = sys.error("Implement me!")

// A test. Uncomment to run.

/*
val sum = letrec(
  'sum,
  Fun('n, If0('n, 0, Add('n, App('sum, Add('n,-1))))),
  App('sum, 10)
)
assert(eval(sum) == NumV(55))
*/

// So how about mutually recursive functions?
//
//   even 0 = 0            // means true  in If0-expressions
//   even n = odd (n - 1)
//
//   odd  0 = 1            // means false in If0-expressions
//   odd  n = even (n - 1)
