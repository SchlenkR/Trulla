# Trulla: How it works internalls

The implementation of the tempalte provider might be interesting, because it contains (in a simple form) the building blocks that are required for a programming language. It has:

**A parser** [Parsing.fs](src/TypeProvider/Trulla/Parsing.fs) implemented with FParsec. The parser output is a sequence of tokens:

```fsharp
type Token =
    | Text of string
    | Hole of PVal<MemberToken>
    | For of ident: PVal<string> * exp: PVal<MemberToken>
    | If of PVal<MemberToken>
    | Else
    | End
    
and MemberToken =
    | AccessToken of {| instanceExp: PVal<MemberToken>; memberName: string |}
    | IdentToken of string
```

**An untyped AST** [Ast.fs](src/TypeProvider/Trulla/Ast.fs) that gets constructed from the parsed token sequence:

```fsharp

type TVar =
    | Root
    | TVar of int

type private BindingContext = Map<string, TVar>

type TVal<'a> =
    { 
        range: Range
        tvar: TVar
        bindingContext: BindingContext
        value: 'a 
    }
    override this.ToString() = sprintf "(%A)%A" this.range this.value

type TExp =
    | Text of string
    | Hole of TVal<MemberExp>
    | For of ident: TVal<string> * exp: TVal<MemberExp> * body: TExp list
    | If of cond: TVal<MemberExp> * body: TExp list
    | Else of cond: TVal<MemberExp> * body: TExp list

and Body = BindingContext * TExp list

and MemberExp =
    | AccessExp of {| instanceExp: TVal<MemberExp>; memberName: string |}
    | IdentExp of string

type Typ =
    | Mono of string
    | Poly of name: string * typParam: Typ
    | Field of Field
    | Record of TVar
    | Var of TVar

and Field = 
    { 
        name: string
        typ: Typ
    }
```

**A solver** [Solver.fs](src/TypeProvider/Trulla/Solver.fs) that types records and identifiers of the AST

```fsharp
type RecordDef =
    {
        id: TVar
        fields: Field list
        name: string
    }
```

**A generator (renderer)** [ReflectionRenderer.fs](src/TypeProvider/Trulla/ReflectionRenderer.fs) that transforms all the previous into the final string.
