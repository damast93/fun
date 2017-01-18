// Le wild demo program

data Bool

fun true = Bool (\x y . x)
fun false = Bool (\x y . y)

fun if cond truepart falsepart = 
    (Bool! cond) truepart falsepart ()

fun main = {
    print "All I'm saying is ...";
    if true 
        { print "Hello" }
        { print "Bye" }
}