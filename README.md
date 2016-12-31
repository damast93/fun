# *Fun* - a minimalist toy compiler 

The goal of *Fun* is to create a minimal [bootstrapping](https://en.wikipedia.org/wiki/Bootstrapping_(compilers)) compiler *for* and *in* a nontrivial toy language. *Fun* is conceived as the simplest programming language in which we can have fun tackling a larger problem.

*Fun* thus follows a decided minimalist philosophy. Large parts of its core functionality and language constructs are actually implemented as library functions on the basis of few, very simple features.

The implementation prioritizes correctness and education value over anything else, like efficiency. The project includes a compiler `func` targeting pure portable C, and an interactive interpreter `funny`. Here are some features of what *Fun* can do

* full garbage collection
* first class and higher-order functions
* lexical scope, anonymous functions, closures
* basic support for managed arrays, string, numbers and I/O
* user-defined datatypes
* tail-call elimination

Roughly, *Fun* is strict untyped lambda calculus with some mostly syntactic extensions and ML-inspired look. *Fun* is not purely functional, so side effects are always possible. There is no static typechecking but types are distinguished strongly at runtime.

Both `funny` and the first-stage of `func` are implemented in F#. The second-stage, eventually, will be written in *Fun* itself.

# A demo program

    // Good old functional quick sort

    fun quicksort list = 
        if (<= (length list) 1)
          { list }
          { let pivot = head list in 
            let rest  = tail list in 
            let left  = filter (> pivot) rest in
            let right = filter (<= pivot) rest in
            concat (quicksort left) (cons pivot (quicksort right))
          }
        

    fun main = { 
	    let list = [3, 1, 6, 4, 5] in
	
	    for (quicksort list) (\a . 
	        print(str a);
	        print("\n")
	    )
	}

# Core language

## Functional basics

The core-part of *Fun* is minimalistic and held super easy. The basic constructions come from lambda calculus, creating anonymous functions with full currying and scoping rules as usual, e.g.

    \x . x, \f x . f x

Function application can be written without brackets, associating to the left. Expressions are evaluated from left to right.


 Variables and functions (the same thing!) can be bound using `let` like in

    let k = 5 in ...

and

    let fact n = product (range 1 n) in fact k

For recursive nested functions, there is `letrec`

    letrec f x = f x in ...

Top-Level values can be defined using the keyword `fun`

    fun isPrime n = forall (\d . == 0 (mod n d)) (range 2 (- n 1))

Recursion is automatically allowed here. The syntax is free form, excess whitespace is insignificant and there is no distinction between function and operator names, so

    fun plus = +

is perfectly fine. On the downside, there is no inherent support for infix operator placement.

## Datatypes

The core datatypes are `int`, `float`, `lambda`, `array` and `unit`. 

All further data structures can be created from these (see [Church encoding](https://en.wikipedia.org/wiki/Church_encoding)). This means that basically every object in *Fun* is in principle a callable function. In the absence of static typechecking, this has to lead to terribly confusing error scenarios. Therefore, *Fun* allows the wrapping of data into user-datatypes.

    data Bool
	
	fun true = Bool (\x y . x)
	fun false = Bool (\x y . y)

    fun if cond truepart falsepart = 
        (Bool? cond) truepart falsepart ()

    fun main = {
		if true 
			{ print "Hello" }
			{ print "Bye" }
	}

Following the minimalist philosophy, we have defined boolean values and conditionals by library functions only. Defining our new datatype by

    data Bool

does the following things for us: It defines a function `Bool` that wraps any object into a new dedicated user-object of type `Bool`. And it allows us to unwrap again using `Bool?`, giving us back the underlying object. As we wanted

    true 1 2 

results in an error, as `true` not a callable `lambda` but just a `Bool` object.     

# Sweet sweet extensions

That's it, that's all the core features we need. The rest is syntactic sugar just to make life easier and prettier.

## Delay

For convenience, there is a syntax for delayed computation

    { computation }

which is equivalent to `\() . computation`. We have already seen that syntax in combination with `if`, and it proves invaluable for defining control structures.

## Lists and strings

Strings are defined in the standard library as a user-type `String`, building on top of arrays of `int`s. There is syntactic support for them though

    "Hello, World"

The same goes for lists, which look like this

    [1,2, ["Hello", 3]]

The above just reduced to `cons`-cells defined by the standard library (type `List`)

    cons 1 (cons 2 (cons (cons "Hello" (cons 3 nil)) nil))

## Sequencing

As we'll sometimes want to chain impure computations, the sequencing operator `;` can used like

    fun demo n = 
        print "Hi";
        print "The result is: ";
        print (str n)

## Preprocessing

Including other files

    include "std.fun"

## Sugar all the way

I lied calling `let` a core-feature. We can be more minimalist than that. Even that is just sugar for some lambda magic. For example instead of 

    let f x = x + 1 in f 41

we just rewrite it as

    (\f . f 41) (\x . x + 1)

Even `letrec` can be expressed that way (in a trickier fashion), and of course 

    \x y . function-body

is the same as the double-lambda

    \x . \y . function-body

This way, the only core features of *Fun* are most basic lambda stuff

    \x . body, x y

user-types

    data List

and top-level bindings

    fun f x = x + 1

Looks pretty *fun*, right?