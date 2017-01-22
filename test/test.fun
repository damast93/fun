// Good old functional quick sort

fun quicksort list = 
    if (length list <= 1)
      { list }
      { let pivot = head list in 
        let rest  = tail list in 
        let left  = filter (\x . x < pivot) rest in
        let right = filter (\x . x >= pivot) rest in
        concat (quicksort left) (cons pivot (quicksort right))
      }


fun main = { 
    let list = [3, 1, 6, 4, 5, 42, 11] in

    printn "Sorting a list: ";
    for (quicksort list) (\a . 
        printn(str a)
    )
}