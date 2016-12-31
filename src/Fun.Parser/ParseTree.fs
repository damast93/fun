namespace Fun.Parser

type internal Ident    = Fun.Syntax.Ident
type internal Typename = Fun.Syntax.Typename

type internal Term = 
    | Unit
    | Int    of int
    | List   of Term
    | Float  of float
    | String of string
    
    | Identifier  of Ident
    | Lambda      of Ident list * Term
    | Application of Term * Term
    | Sequence    of Term list
    | Delay       of Term
    | Let         of Ident * Ident list * Term * Term
    | LetRec      of Ident * Ident list * Term * Term

type internal Declaration = 
    | Fun  of Ident * Ident list * Term
    | Data of Typename

type internal Program = Program of Declaration list