namespace Fun.Syntax

type Ident = string
type Typename = string

type Expression = 
    | Unit
    | Int         of int
    | Float       of float
    | String      of string

    | Var         of Ident
    | Global      of Ident
    | Lambda      of Ident * Expression
    | Application of Expression * Expression

type Declaration = Fun of Ident * Expression

type Program = { usertypes : Typename list; declarations : Declaration list }
