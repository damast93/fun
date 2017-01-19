namespace Fun.Syntax

type Ident = string
type Typename = string

type Expression = 
    | Unit
    | Int            of int
    | Float          of float
    | String         of string
    
    | Definition     of Ident
    | Identifier     of Ident

    | Lambda         of Ident * Expression
    | Application    of Expression * Expression
    | Sequence       of Expression list
    | LetIn          of Ident * Expression * Expression
    | LetRecIn       of Ident * Expression * Expression

type Definition = Fun of Ident * Expression

type Module = { usertypes : Typename list; definitions : Definition list }