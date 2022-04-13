namespace Languages

module CPWriter =

    let p = printf "%s"
    let writeNumber (n:CPAst.number) = printf "%d" n
    let writeCharacter (n:CPAst.character) = printf "%A" n
    let inline (++) (f: 'a -> unit) (f2: 'b -> unit) (v,v2) : unit = f v; f2 v2
    let inline (|+>) (arg: 'a * 'b) (f: 'a -> unit) : 'b = fst arg|> f; snd arg
    let inline (+<) (f: 'a -> unit) (s: string) v : unit = f v; p s
    let inline (+>) (s: string) f2 v : unit = p s; f2 v
    let optional f = Option.iter f
    let inList f = List.iter f

    let inOneOrManyList joinStr f = CPAst.iter f (fun () -> p joinStr) 
    let writeIdent(ast:CPAst.ident) = printf "%s" ast

    let writeImportList (ast: CPAst.ImportList) : unit = inOneOrManyList "," ("IMPORT " +> (optional (writeIdent +< ":=")) ++ writeIdent) ast
    let rec writeIdentList(ast:CPAst.IdentList) : unit = (inOneOrManyList "," writeIdentDef) ast
    and writeIdentDef(ast:CPAst.IdentDef) : unit = (writeIdent ++ optional (fun x->match x with | CPAst.IdentType.Export-> p "*" | CPAst.IdentType.ReadOnlyOrImplementOnly -> p "-")) ast
    and writeConstExpr(ast:CPAst.ConstExpr) : unit = writeExpr ast
    and writeQualident(ast:CPAst.Qualident) : unit = (optional (writeIdent +< ".") ++ writeIdent) ast
    and writeFieldList(ast:CPAst.FieldList) : unit = optional (writeIdentList +< ":" ++ writeType) ast
    and writeType(ast:CPAst.Type) : unit = 
        match ast with 
        | CPAst.Type.Simple(qualident) -> writeQualident qualident
        | CPAst.Type.ARRAY(constExprOneOrManyOption, tp) -> ("ARRAY" +> (optional (inOneOrManyList "," writeConstExpr)) ++ ("OF" +> writeType)) (constExprOneOrManyOption, tp)
        | CPAst.Type.RECORD(prefix, record) -> 
            let writePrefix prefix =
                match prefix with  
                 | CPAst.RecordPrefix.ABSTRACT -> p "ABSTRACT"
                 | CPAst.RecordPrefix.EXTENSIBLE -> p "EXTENSIBLE"
                 | CPAst.RecordPrefix.LIMITED -> p "LIMITED"
            let f = (optional writePrefix +< "RECORD" ++ ((optional ("(" +> writeQualident +< ")")) ++ ((inOneOrManyList ";" writeFieldList) +< "END"))) 
            f (prefix, record)
        | CPAst.Type.POINTER(tp) -> ("POINTER TO" +> writeType) tp
        | CPAst.Type.PROCEDURE(formalPars) -> ("PROCEDURE" +> optional writeFormalPars) formalPars
    and writeConstDecl(ast:CPAst.ConstDecl) = ((writeIdentDef +< "=") ++ writeConstExpr) ast
    and writeTypeDecl(ast:CPAst.TypeDecl) = ((writeIdentDef +< "=") ++ writeType) ast
    and writeVarDecl(ast:CPAst.VarDecl) = ((writeIdentList +< ":") ++ writeType) ast
    and writeReciever(ast:CPAst.Receiver) : unit= 
        let writePrefix prefix =
            match prefix with  
            | CPAst.RecieverPrefix.IN -> p "IN"
            | CPAst.RecieverPrefix.VAR -> p "VAR"
        ("(" +> (optional writePrefix) ++ ((writeIdent +< ":") ++ (writeIdent +< ")"))) ast
    and writeFPSection(ast:CPAst.FPSection) : unit= 
        let writePrefix prefix =
            match prefix with  
            | CPAst.FPSectionPrefix.IN -> p "IN"
            | CPAst.FPSectionPrefix.OUT -> p "OUT"
            | CPAst.FPSectionPrefix.VAR -> p "VAR"
        (optional writePrefix ++ ((inOneOrManyList "," writeIdent) ++ (":" +>writeType))) ast
    and writeFormalPars(ast:CPAst.FormalPars) : unit = 
        ("(" +> (optional (inOneOrManyList ";" writeFPSection)) +< ")" ++ optional (":" +> writeType)) ast
    and writeMethAttributes(ast:CPAst.MethAttributes) : unit= 
        let writeNew (n:CPAst.ProcNew) : unit =  p ", NEW"
        let writeKind (k:CPAst.ProcKind) : unit=
            match k with  
            | CPAst.ProcKind.Abstract -> p "ABSTRACT"
            | CPAst.ProcKind.Empty -> p "EMPTY"
            | CPAst.ProcKind.Extensible -> p "EXTENSIBLE"
        (optional writeNew ++ optional writeKind) ast
    and writeForwardDecl(ast:CPAst.ForwardDecl) : unit = 
        (("PROCEDURE" + " ^ ") +> (optional writeReciever) ++ (writeIdentDef ++ ((optional writeFormalPars) ++ writeMethAttributes))) ast
    and writeNullTerm(ast:CPAst.NullTerm) : unit = p "$"
    and writeDesignatorOps(ast:CPAst.DesignatorOps) : unit = 
        match ast with
        | CPAst.DesignatorOps.Ident(i) -> ("." +> writeIdent) i
        | CPAst.DesignatorOps.Index(a1,a2) -> ("[" +> writeExprList +<  "]") (a1,a2)
        | CPAst.DesignatorOps.Ref ->  p "^" 
        | CPAst.DesignatorOps.FnQ(q) -> ("("  +> writeQualident +<  ")") q
        | CPAst.DesignatorOps.FnE(e) -> ("(" +> (optional writeExprList) +< ")") e
    and writeDesignator(ast:CPAst.Designator) : unit = 
        (writeQualident ++ (inList writeDesignatorOps ++ (optional writeNullTerm))) ast
    and writeMulOp(ast:CPAst.MulOp) : unit = 
        match ast with 
        | CPAst.MulOp.Division -> p "/"
        | CPAst.MulOp.MUL -> p "*"
        | CPAst.MulOp.DIV -> p "DIV"
        | CPAst.MulOp.MOD -> p "MOD"
        | CPAst.MulOp.AND -> p "&"
    and writeFactor(ast:CPAst.Factor) : unit = 
        match ast with
        | CPAst.Factor.Designator(d) -> writeDesignator d
        | CPAst.Factor.Number(n) -> writeNumber n
        | CPAst.Factor.Char(c) -> writeCharacter c
        | CPAst.Factor.String(s) -> p s
        | CPAst.Factor.NIL -> p "NIL"
        | CPAst.Factor.Set(s) -> writeSet s
        | CPAst.Factor.Expr(e) -> ("(" +> writeExpr +< ")") e
        | CPAst.Factor.FactorF(f) -> (" ~ " +> writeFactor) f
    and writeTerm(ast:CPAst.Term) : unit = (writeFactor ++ inList (writeMulOp ++ writeFactor)) ast
    and writeAddOp(ast:CPAst.AddOp) : unit = 
        match ast with 
        | CPAst.AddOp.Plus -> p "+"
        | CPAst.AddOp.Minus -> p "-"
        | CPAst.AddOp.OR -> p "OR"
    and writeSimpleExpr(ast:CPAst.SimpleExpr) : unit = 
        let writePrefix pref =
            match pref with 
            | CPAst.SimpleExprPrefix.Plus -> p "+"
            | CPAst.SimpleExprPrefix.Minus -> p "-"
        (optional writePrefix ++ (writeTerm ++ inList (writeAddOp ++ writeTerm))) ast
    and writeRelation(ast:CPAst.Relation) : unit =  
        match ast with
        | CPAst.Relation.Eq -> p "="
        | CPAst.Relation.NotEq -> p "#"
        | CPAst.Relation.More -> p ">"
        | CPAst.Relation.MoreOrEq -> p ">="
        | CPAst.Relation.Less -> p "<"
        | CPAst.Relation.LessOrEq -> p "<="
        | CPAst.Relation.IN -> p "IN"
        | CPAst.Relation.IS -> p "IS"
    and writeExpr(ast:CPAst.Expr) : unit = (writeSimpleExpr ++ optional (writeRelation ++ writeSimpleExpr)) ast
    and writeExprList(ast:CPAst.ExprList) : unit = (inOneOrManyList "," writeExpr) ast
    and writeElement(ast:CPAst.Element) : unit = (inOneOrManyList ".." writeExpr) ast
    and writeSet(ast:CPAst.Set) : unit = ("{" +> optional (inOneOrManyList "," writeElement) +< "}") ast
    and writeCaseLabels(ast:CPAst.CaseLabels) : unit = inOneOrManyList ".." writeConstExpr ast
    and writeCase(ast:CPAst.Case) : unit = 
        optional (inOneOrManyList "," writeCaseLabels +< ":" ++ writeStatementSeq) ast
    and writeGuard(ast:CPAst.Guard) : unit = (writeQualident +< ":" ++ writeQualident) ast
    and writeStatementInt(ast:CPAst.StatementInt) : unit = 
        match ast with
        | CPAst.StatementInt.Assignment(f,s)-> (writeDesignator +< ":=" ++ writeExpr) (f,s)
        | CPAst.StatementInt.Fn(d, e)-> (writeDesignator ++ (optional ("(" +> (optional writeExprList) +< ")"))) (d,e)
        | CPAst.StatementInt.IF(main, elses) -> 
            let writeElsIf =  inList ("ELSIF" +> writeExpr +< "THEN" ++ writeStatementSeq)
            let writeElse =  optional ("ELSE" +> writeStatementSeq)
            let writeIf = ("IF" +> writeExpr +< "THEN" ++ writeStatementSeq) ++ (writeElsIf ++ writeElse +< "END")
            writeIf (main, elses)
        | CPAst.StatementInt.CASE(expr, body)->
            ("CASE" +> writeExpr +< "OF" ++ ((inOneOrManyList "|" writeCase) ++ optional ("ELSE" +> writeStatementSeq) +< "END")) (expr, body)
        | CPAst.StatementInt.WHILE(expr, statementSeq)-> ("WHILE" +> writeExpr +< "DO" ++ writeStatementSeq +< "END") (expr, statementSeq)
        | CPAst.StatementInt.REPEAT(statementSeq, expr)-> ("REPEAT" +> writeStatementSeq +< "UNTIL" ++ writeExpr ) (statementSeq, expr)
        | CPAst.StatementInt.FOR(ident, body)->
            ("FOR" +> writeIdent +<  ":=" ++ (writeExpr +<"TO" ++ (writeExpr ++ (optional("BY" +> writeConstExpr) +< "DO" ++ writeStatementSeq +< "END")))) (ident, body)
        | CPAst.StatementInt.LOOP(statementSeq)-> ("LOOP" +> writeStatementSeq +< "END") statementSeq
        | CPAst.StatementInt.WITH(a,b)->
            let f = ("WITH" +> (optional ( writeGuard +< "DO" ++ writeStatementSeq )) 
                        ++ (inList ("|" +> optional(writeGuard +< "DO" ++ writeStatementSeq ))
                        ++ (optional ("ELSE" +> writeStatementSeq)) +< "END"))
            f(a,b)
        | CPAst.StatementInt.EXIT-> p "EXIT"
        | CPAst.StatementInt.RETURN(expr)-> ("RETURN" +> (optional writeExpr)) expr
     and writeStatement(ast:CPAst.Statement) : unit = optional writeStatementInt ast
    and writeStatementSeq (ast: CPAst.StatementSeq) : unit = (inOneOrManyList ";" writeStatement) ast
    and writeDeclSeq1 (ast: CPAst.DeclSeq1) = 
        match ast with  
        | CPAst.DeclSeq1.CONST(cdl) -> inList (writeConstDecl +< ";") cdl
        | CPAst.DeclSeq1.TYPE(tdl)-> inList (writeTypeDecl +< ";") tdl
        | CPAst.DeclSeq1.VAR(vdl)-> inList (writeVarDecl +< ";") vdl
    and writeDeclSeq2 (ast: CPAst.DeclSeq2) = 
        match ast with  
        | CPAst.DeclSeq2.Forward(dl) -> writeForwardDecl dl
        | CPAst.DeclSeq2.Proc(dl)-> writeProcDecl dl
    and writeDeclSeq (ast: CPAst.DeclSeq) : unit= (inList writeDeclSeq1 ++ inList writeDeclSeq2) ast
    and writeProcDecl((receiver, identDef, formalPars, methAttributes, body):CPAst.ProcDecl) : unit= 
        ("PROCEDURE " +> (optional writeReciever)) receiver
        writeIdentDef identDef
        optional writeFormalPars formalPars
        writeMethAttributes methAttributes

        optional (";" +> writeDeclSeq ++ (optional (" BEGIN " +> writeStatementSeq) +< " END " ++ writeIdent)) body
    and writeModule ((ident, importList, declSeq, beginStatementSeq, endstatementSeq): CPAst.Module) =
        ("MODULE " +> writeIdent +< ";") ident
        optional writeImportList importList
        writeDeclSeq declSeq
        optional ("BEGIN" +> writeStatementSeq) beginStatementSeq
        optional ("CLOSE" +> writeStatementSeq) endstatementSeq
        ("END " +> writeIdent) ident

