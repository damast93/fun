namespace Fun.Parser

type Ident    = string
type Typename = string

type Term = 
    | Unit
    | Int    of int
    | Float  of float
    | List   of Term list
    | String of string
    
    | Identifier  of Ident
    | Lambda      of Ident list * Term
    | Application of Term list
    | Sequence    of Term list
    | Delay       of Term
    | Let         of Ident * Ident list * Term * Term
    | LetRec      of Ident * Ident list * Term * Term

type Definition = 
    | Fun  of Ident * Ident list * Term
    | Data of Typename

type Module = Module of Definition list