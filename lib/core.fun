// Basic datatypes

data Char
data List
data String
data Ref

// Boolean logic

fun if cond truepart falsepart = 
    (Bool! cond) truepart falsepart ()

fun (&&) a b = if a { b } { false }
fun (||) a b = if a { true } { b }
fun not a = if a { false } { true }
    
// Arithmetic

fun (<) a b = (a <= b) && not (a == b)
fun (>) a b = (b <= a) && not (a == b)
fun (>=) a b = (b <= a)
fun (!=) a b = not (a == b)

// Lists

fun nil = List(())
fun cons x xs = List! xs; List(\f . f x xs)

fun head list = 
    let fn = List! list in
    fn (\x xs . x)

fun tail list = 
    let fn = List! list in
    if (unit? fn) 
        { error "tail: empty list" }
        { fn (\x xs . xs) }

fun nil? list = 
    let c = List! list in
    unit? c

// Higher order list functions

fun length list = 
    if (nil? list)
        { 0 }
        { 1 + length (tail list) }

fun foldl f acc list = 
    if (nil? list) 
        { acc }
        { foldl f (f (head list) acc) (tail list) }

fun reverse = foldl cons nil

fun map f list = 
    if (nil? list)
        { nil }
        { cons (f (head list)) (map f (tail list)) }

fun filter pred list = 
    if (nil? list)
        { nil }
        { let h = head list in
          if (pred h)
              { cons h (filter pred (tail list)) }
              { filter pred (tail list) } }

fun for list action = 
    if (nil? list)
        { () }
        { action (head list); for (tail list) action }

fun concat list rest = 
    letrec concatRev rev rest = 
        if (nil? rev)
            { rest }
            { concatRev (tail rev) (cons (head rev) rest) } in
    concatRev (reverse list) rest