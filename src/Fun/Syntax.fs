namespace Fun.Syntax

type Ident = string
type Typename = string

type Expression = 
    | Unit
    | Int            of int
    | Float          of float
    | String         of string

    | Identifier     of Ident
    | CoreIdentifier of Ident
    | Lambda         of Ident * Expression
    | Application    of Expression * Expression
    | LetIn          of Ident * Expression * Expression
    | LetRecIn       of Ident * Expression * Expression

type Definition = Fun of Ident * Expression

type Program = { usertypes : Typename list; defintions : Definition list }
