namespace Fun.Semantics

type Value = 
    | Unit
    | Int of int
    | Float of float
    | String of string // TODO replace by array of chars
    | Array of Value[]
    | Func of (Value -> Value)
    | UserType of Value * string
    with
        override this.ToString() = 
            match this with
            | Unit -> "()"
            | Int(n) -> sprintf "%i" n
            | Float(f) -> sprintf "%f" f
            | String(s) -> s
            | Array(arr) -> sprintf "<array[0..%i]>" (Array.length arr)
            | Func(f) -> sprintf "<function>"
            | UserType(x,typename) -> sprintf "<%s>" typename