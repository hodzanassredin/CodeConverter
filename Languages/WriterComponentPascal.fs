namespace Languages

module CPWriter =

    let p = printf "%s"
    let writeNumber (n:AstCP.number) = printf "%d" n
    let writeCharacter (n:AstCP.character) = printf "%A" n
    let inline (++) (f: 'a -> unit) (f2: 'b -> unit) (v,v2) : unit = f v; f2 v2
    let inline (|+>) (arg: 'a * 'b) (f: 'a -> unit) : 'b = fst arg|> f; snd arg
    let inline (+<) (f: 'a -> unit) (s: string) v : unit = f v; p s
    let inline (+>) (s: string) f2 v : unit = p s; f2 v
    let optional f = Option.iter f
    let inList f = List.iter f

    let inOneOrManyList joinStr f = AstCP.iter f (fun () -> p joinStr) 
    let writeIdent(ast:AstCP.ident) = printf "%s" ast

    let writeImportList (ast: AstCP.ImportList) : unit = inOneOrManyList "," ("IMPORT " +> (optional (writeIdent +< ":=")) ++ writeIdent) ast
    let rec writeIdentList(ast:AstCP.IdentList) : unit = (inOneOrManyList "," writeIdentDef) ast
    and writeIdentDef(ast:AstCP.IdentDef) : unit = (writeIdent ++ optional (fun x->match x with | AstCP.IdentType.Export-> p "*" | AstCP.IdentType.ReadOnlyOrImplementOnly -> p "-")) ast
    and writeConstExpr(ast:AstCP.ConstExpr) : unit = writeExpr ast
    and writeQualident(ast:AstCP.Qualident) : unit = (optional (writeIdent +< ".") ++ writeIdent) ast
    and writeFieldList(ast:AstCP.FieldList) : unit = optional (writeIdentList +< ":" ++ writeType) ast
    and writeType(ast:AstCP.Type) : unit = 
        match ast with 
        | AstCP.Type.Simple(qualident) -> writeQualident qualident
        | AstCP.Type.ARRAY(constExprOneOrManyOption, tp) -> ("ARRAY" +> (optional (inOneOrManyList "," writeConstExpr)) ++ ("OF" +> writeType)) (constExprOneOrManyOption, tp)
        | AstCP.Type.RECORD(prefix, record) -> 
            let writePrefix prefix =
                match prefix with  
                 | AstCP.RecordPrefix.ABSTRACT -> p "ABSTRACT"
                 | AstCP.RecordPrefix.EXTENSIBLE -> p "EXTENSIBLE"
                 | AstCP.RecordPrefix.LIMITED -> p "LIMITED"
            let f = (optional writePrefix +< "RECORD" ++ ((optional ("(" +> writeQualident +< ")")) ++ ((inOneOrManyList ";" writeFieldList) +< "END"))) 
            f (prefix, record)
        | AstCP.Type.POINTER(tp) -> ("POINTER TO" +> writeType) tp
        | AstCP.Type.PROCEDURE(formalPars) -> ("PROCEDURE" +> optional writeFormalPars) formalPars
    and writeConstDecl(ast:AstCP.ConstDecl) = ((writeIdentDef +< "=") ++ writeConstExpr) ast
    and writeTypeDecl(ast:AstCP.TypeDecl) = ((writeIdentDef +< "=") ++ writeType) ast
    and writeVarDecl(ast:AstCP.VarDecl) = ((writeIdentList +< ":") ++ writeType) ast
    and writeReciever(ast:AstCP.Receiver) : unit= 
        let writePrefix prefix =
            match prefix with  
            | AstCP.RecieverPrefix.IN -> p "IN"
            | AstCP.RecieverPrefix.VAR -> p "VAR"
        ("(" +> (optional writePrefix) ++ ((writeIdent +< ":") ++ (writeIdent +< ")"))) ast
    and writeFPSection(ast:AstCP.FPSection) : unit= 
        let writePrefix prefix =
            match prefix with  
            | AstCP.FPSectionPrefix.IN -> p "IN"
            | AstCP.FPSectionPrefix.OUT -> p "OUT"
            | AstCP.FPSectionPrefix.VAR -> p "VAR"
        (optional writePrefix ++ ((inOneOrManyList "," writeIdent) ++ (":" +>writeType))) ast
    and writeFormalPars(ast:AstCP.FormalPars) : unit = 
        ("(" +> (optional (inOneOrManyList ";" writeFPSection)) +< ")" ++ optional (":" +> writeType)) ast
    and writeMethAttributes(ast:AstCP.MethAttributes) : unit= 
        let writeNew (n:AstCP.ProcNew) : unit =  p ", NEW"
        let writeKind (k:AstCP.ProcKind) : unit=
            match k with  
            | AstCP.ProcKind.Abstract -> p "ABSTRACT"
            | AstCP.ProcKind.Empty -> p "EMPTY"
            | AstCP.ProcKind.Extensible -> p "EXTENSIBLE"
        (optional writeNew ++ optional writeKind) ast
    and writeForwardDecl(ast:AstCP.ForwardDecl) : unit = 
        (("PROCEDURE" + " ^ ") +> (optional writeReciever) ++ (writeIdentDef ++ ((optional writeFormalPars) ++ writeMethAttributes))) ast
    and writeNullTerm(ast:AstCP.NullTerm) : unit = p "$"
    and writeDesignatorOps(ast:AstCP.DesignatorOps) : unit = 
        match ast with
        | AstCP.DesignatorOps.Ident(i) -> ("." +> writeIdent) i
        | AstCP.DesignatorOps.Index(a1,a2) -> ("[" +> writeExprList +<  "]") (a1,a2)
        | AstCP.DesignatorOps.Ref ->  p "^" 
        | AstCP.DesignatorOps.FnQ(q) -> ("("  +> writeQualident +<  ")") q
        | AstCP.DesignatorOps.FnE(e) -> ("(" +> (optional writeExprList) +< ")") e
    and writeDesignator(ast:AstCP.Designator) : unit = 
        (writeQualident ++ (inList writeDesignatorOps ++ (optional writeNullTerm))) ast
    and writeMulOp(ast:AstCP.MulOp) : unit = 
        match ast with 
        | AstCP.MulOp.Division -> p "/"
        | AstCP.MulOp.MUL -> p "*"
        | AstCP.MulOp.DIV -> p "DIV"
        | AstCP.MulOp.MOD -> p "MOD"
        | AstCP.MulOp.AND -> p "&"
    and writeFactor(ast:AstCP.Factor) : unit = 
        match ast with
        | AstCP.Factor.Designator(d) -> writeDesignator d
        | AstCP.Factor.Number(n) -> writeNumber n
        | AstCP.Factor.Char(c) -> writeCharacter c
        | AstCP.Factor.String(s) -> p s
        | AstCP.Factor.NIL -> p "NIL"
        | AstCP.Factor.Set(s) -> writeSet s
        | AstCP.Factor.Expr(e) -> ("(" +> writeExpr +< ")") e
        | AstCP.Factor.FactorF(f) -> (" ~ " +> writeFactor) f
    and writeTerm(ast:AstCP.Term) : unit = (writeFactor ++ inList (writeMulOp ++ writeFactor)) ast
    and writeAddOp(ast:AstCP.AddOp) : unit = 
        match ast with 
        | AstCP.AddOp.Plus -> p "+"
        | AstCP.AddOp.Minus -> p "-"
        | AstCP.AddOp.OR -> p "OR"
    and writeSimpleExpr(ast:AstCP.SimpleExpr) : unit = 
        let writePrefix pref =
            match pref with 
            | AstCP.SimpleExprPrefix.Plus -> p "+"
            | AstCP.SimpleExprPrefix.Minus -> p "-"
        (optional writePrefix ++ (writeTerm ++ inList (writeAddOp ++ writeTerm))) ast
    and writeRelation(ast:AstCP.Relation) : unit =  
        match ast with
        | AstCP.Relation.Eq -> p "="
        | AstCP.Relation.NotEq -> p "#"
        | AstCP.Relation.More -> p ">"
        | AstCP.Relation.MoreOrEq -> p ">="
        | AstCP.Relation.Less -> p "<"
        | AstCP.Relation.LessOrEq -> p "<="
        | AstCP.Relation.IN -> p "IN"
        | AstCP.Relation.IS -> p "IS"
    and writeExpr(ast:AstCP.Expr) : unit = (writeSimpleExpr ++ optional (writeRelation ++ writeSimpleExpr)) ast
    and writeExprList(ast:AstCP.ExprList) : unit = (inOneOrManyList "," writeExpr) ast
    and writeElement(ast:AstCP.Element) : unit = (inOneOrManyList ".." writeExpr) ast
    and writeSet(ast:AstCP.Set) : unit = ("{" +> optional (inOneOrManyList "," writeElement) +< "}") ast
    and writeCaseLabels(ast:AstCP.CaseLabels) : unit = inOneOrManyList ".." writeConstExpr ast
    and writeCase(ast:AstCP.Case) : unit = 
        optional (inOneOrManyList "," writeCaseLabels +< ":" ++ writeStatementSeq) ast
    and writeGuard(ast:AstCP.Guard) : unit = (writeQualident +< ":" ++ writeQualident) ast
    and writeStatementInt(ast:AstCP.StatementInt) : unit = 
        match ast with
        | AstCP.StatementInt.Assignment(f,s)-> (writeDesignator +< ":=" ++ writeExpr) (f,s)
        | AstCP.StatementInt.Fn(d, e)-> (writeDesignator ++ (optional ("(" +> (optional writeExprList) +< ")"))) (d,e)
        | AstCP.StatementInt.IF(main, elses) -> 
            let writeElsIf =  inList ("ELSIF" +> writeExpr +< "THEN" ++ writeStatementSeq)
            let writeElse =  optional ("ELSE" +> writeStatementSeq)
            let writeIf = ("IF" +> writeExpr +< "THEN" ++ writeStatementSeq) ++ (writeElsIf ++ writeElse +< "END")
            writeIf (main, elses)
        | AstCP.StatementInt.CASE(expr, body)->
            ("CASE" +> writeExpr +< "OF" ++ ((inOneOrManyList "|" writeCase) ++ optional ("ELSE" +> writeStatementSeq) +< "END")) (expr, body)
        | AstCP.StatementInt.WHILE(expr, statementSeq)-> ("WHILE" +> writeExpr +< "DO" ++ writeStatementSeq +< "END") (expr, statementSeq)
        | AstCP.StatementInt.REPEAT(statementSeq, expr)-> ("REPEAT" +> writeStatementSeq +< "UNTIL" ++ writeExpr ) (statementSeq, expr)
        | AstCP.StatementInt.FOR(ident, body)->
            ("FOR" +> writeIdent +<  ":=" ++ (writeExpr +<"TO" ++ (writeExpr ++ (optional("BY" +> writeConstExpr) +< "DO" ++ writeStatementSeq +< "END")))) (ident, body)
        | AstCP.StatementInt.LOOP(statementSeq)-> ("LOOP" +> writeStatementSeq +< "END") statementSeq
        | AstCP.StatementInt.WITH(a,b)->
            let f = ("WITH" +> (optional ( writeGuard +< "DO" ++ writeStatementSeq )) 
                        ++ (inList ("|" +> optional(writeGuard +< "DO" ++ writeStatementSeq ))
                        ++ (optional ("ELSE" +> writeStatementSeq)) +< "END"))
            f(a,b)
        | AstCP.StatementInt.EXIT-> p "EXIT"
        | AstCP.StatementInt.RETURN(expr)-> ("RETURN" +> (optional writeExpr)) expr
     and writeStatement(ast:AstCP.Statement) : unit = optional writeStatementInt ast
    and writeStatementSeq (ast: AstCP.StatementSeq) : unit = (inOneOrManyList ";" writeStatement) ast
    and writeDeclSeq1 (ast: AstCP.DeclSeq1) = 
        match ast with  
        | AstCP.DeclSeq1.CONST(cdl) -> inList (writeConstDecl +< ";") cdl
        | AstCP.DeclSeq1.TYPE(tdl)-> inList (writeTypeDecl +< ";") tdl
        | AstCP.DeclSeq1.VAR(vdl)-> inList (writeVarDecl +< ";") vdl
    and writeDeclSeq2 (ast: AstCP.DeclSeq2) = 
        match ast with  
        | AstCP.DeclSeq2.Forward(dl) -> writeForwardDecl dl
        | AstCP.DeclSeq2.Proc(dl)-> writeProcDecl dl
    and writeDeclSeq (ast: AstCP.DeclSeq) : unit= (inList writeDeclSeq1 ++ inList writeDeclSeq2) ast
    and writeProcDecl((receiver, identDef, formalPars, methAttributes, body):AstCP.ProcDecl) : unit= 
        ("PROCEDURE " +> (optional writeReciever)) receiver
        writeIdentDef identDef
        optional writeFormalPars formalPars
        writeMethAttributes methAttributes

        optional (";" +> writeDeclSeq ++ (optional (" BEGIN " +> writeStatementSeq) +< " END " ++ writeIdent)) body
    and writeModule ((ident, importList, declSeq, beginStatementSeq, endstatementSeq): AstCP.Module) =
        ("MODULE " +> writeIdent +< ";") ident
        optional writeImportList importList
        writeDeclSeq declSeq
        optional ("BEGIN" +> writeStatementSeq) beginStatementSeq
        optional ("CLOSE" +> writeStatementSeq) endstatementSeq
        ("END " +> writeIdent) ident

