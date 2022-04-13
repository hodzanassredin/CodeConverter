namespace Languages

module CPAst =
    //type SimpleType =
    //    | BOOLEAN// of bool
    //    | SHORTCHAR// of byte
    //    | CHAR// of char
    //    | BYTE// of sbyte
    //    | SHORTINT// of int16
    //    | INTEGER// of int32
    //    | LONGINT// of int64
    //    | SHORTREAL// of System.Single
    //    | REAL// of System.Double
    //    | SET// of byte//max 31
    type OneOrMany<'a> = 'a * 'a list

    let rec iter f fb ((h,t):OneOrMany<_>) =
        f h
        List.iter (fun x -> fb()
                            f x) t
        
    type ident = string
    type number = int
    type character = Char
    type IdentType =
        | Export
        | ReadOnlyOrImplementOnly
    type IdentDef = ident * IdentType option
    type Qualident = ident option * ident
    type IdentList = IdentDef OneOrMany
    type ExprList = Expr OneOrMany
    and NullTerm = bool
    and DesignatorOps = 
        | Ident of ident
        | Index of ExprList
        | Ref
        | FnQ of Qualident
        | FnE of (ExprList option)
    and Designator = Qualident * (DesignatorOps list * NullTerm option)
    and MulOp = 
        | Division
        | MUL
        | DIV
        | MOD
        | AND
    and AddOp = 
        | Plus
        | Minus
        | OR
    and Relation =
        | Eq
        | NotEq
        | More
        | MoreOrEq
        | Less
        | LessOrEq
        | IN
        | IS
    and Element = (Expr OneOrMany)
    and Set = (Element OneOrMany) option
    and Factor = 
        | Designator of Designator
        | Number of number
        | Char of character
        | String of string
        | NIL
        | Set of Set
        | Expr of Expr
        | FactorF of Factor
    and Term = Factor * (MulOp * Factor) list
    and SimpleExprPrefix = Plus | Minus
    and SimpleExpr = (SimpleExprPrefix option) * (Term * (AddOp * Term) list)
    and Expr = SimpleExpr * (Relation * SimpleExpr) option
    and ConstExpr =  Expr
    and Guard = Qualident * Qualident
    and CaseLabels = ConstExpr OneOrMany
    and Case = (CaseLabels OneOrMany * StatementSeq) option
    and Statement = StatementInt option
    and StatementInt = 
        | Assignment of Designator * Expr
        | Fn of Designator * ExprList option option
        | IF of (Expr * StatementSeq) * ((Expr * StatementSeq) list * StatementSeq option)
        | CASE of Expr * (Case OneOrMany * StatementSeq option)
        | WHILE of Expr * StatementSeq
        | REPEAT of StatementSeq * Expr
        | FOR of ident * (Expr * (Expr * (ConstExpr option * StatementSeq)))
        | LOOP of StatementSeq
        | WITH of (Guard * StatementSeq) option * (((Guard * StatementSeq) option) list * StatementSeq option)
        | EXIT
        | RETURN of (Expr option)
    and StatementSeq = Statement OneOrMany
    and FieldList = (IdentList * Type) option
    and RecordPrefix = ABSTRACT | EXTENSIBLE | LIMITED
    and Type = 
        | Simple of Qualident
        | ARRAY of (ConstExpr OneOrMany) option * Type
        | RECORD of RecordPrefix option * (Qualident option * FieldList OneOrMany)
        | POINTER of Type
        | PROCEDURE of FormalPars option
    and RecieverPrefix = VAR | IN
    and Receiver = RecieverPrefix option * (ident * ident)
    and FPSectionPrefix = VAR | IN | OUT
    and FPSection = FPSectionPrefix option * (ident OneOrMany * Type)
    and FormalPars = FPSection OneOrMany option * Type option
    and ForwardDecl = Receiver option * (IdentDef * (FormalPars option * MethAttributes))
    and ProcKind = Abstract | Empty | Extensible
    and ProcNew = unit
    and MethAttributes = ProcNew option * ProcKind option
    and ProcDecl = Receiver option * IdentDef * FormalPars option * MethAttributes * (DeclSeq * (StatementSeq option * ident)) option
    and VarDecl = IdentList * Type
    and TypeDecl = IdentDef * Type
    and ConstDecl = IdentDef * ConstExpr
    and DeclSeq1 = 
        | CONST of ConstDecl list 
        | TYPE of TypeDecl list 
        | VAR of VarDecl list
    and DeclSeq2 = Proc of ProcDecl | Forward of ForwardDecl 
    and DeclSeq = (DeclSeq1 list) * DeclSeq2 list
    and ImportList = (ident option * ident) OneOrMany
    and Module = ident * ImportList option * DeclSeq * StatementSeq option * StatementSeq option



