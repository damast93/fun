// Le wild demo program

fun pyth a b = 
    let a2 = a * a in
    let b2 = b * b in
    a2 + b2

fun main = {
    printn "Hello World from Fun";
    print "Hypothenuse^2 = ";
    print (pyth 3 4)
}